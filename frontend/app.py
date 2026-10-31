import streamlit as st
import pandas as pd
import requests
import matplotlib.pyplot as plt

st.set_page_config(page_title="DID Event Study", layout="wide")
st.title("📊 Difference-in-Differences Event Study")

def stars(p):
    if p < 0.01:
        return "***"
    elif p < 0.05:
        return "**"
    elif p < 0.10:
        return "*"
    else:
        return ""

uploaded_file = st.file_uploader(
    "Upload your panel dataset (CSV format)",
    type=["csv"]
)

if uploaded_file is not None:

    df = pd.read_csv(uploaded_file)

    st.subheader("Data Preview")
    st.dataframe(df.head(), use_container_width=True)

    st.subheader("Select Variables")

    yname  = st.selectbox("Outcome variable (y)", df.columns)
    tname  = st.selectbox("Time variable (t)", df.columns)
    idname = st.selectbox("Unit ID variable", df.columns)
    gname  = st.selectbox("Treatment cohort variable", df.columns)

    validation_errors = []

    if not pd.api.types.is_numeric_dtype(df[yname]):
        validation_errors.append("Outcome variable must be numeric.")
    if not pd.api.types.is_numeric_dtype(df[tname]):
        validation_errors.append("Time variable must be numeric.")
    if df[idname].isna().any():
        validation_errors.append("Unit ID variable contains missing values.")
    if not pd.api.types.is_numeric_dtype(df[gname]):
        validation_errors.append("Treatment cohort variable must be numeric.")

    duplicates = df.duplicated(subset=[idname, tname]).sum()
    if duplicates > 0:
        validation_errors.append(
            f"Dataset contains {duplicates} duplicate (id, time) observations."
        )
    if df[tname].nunique() < 2:
        validation_errors.append("Dataset must contain at least two time periods.")

    if validation_errors:
        st.error("Validation errors detected:")
        for err in validation_errors:
            st.write(f"- {err}")

    if st.button("Run DID Analysis") and not validation_errors:

        payload = {
            "data":   df.to_dict(orient="list"),
            "yname":  yname,
            "tname":  tname,
            "idname": idname,
            "gname":  gname
        }

        with st.spinner("Running DID model (bootstrap + simultaneous bands)..."):
            try:
                response = requests.post(
                    "http://127.0.0.1:5000/api/did",
                    json=payload,
                    timeout=600
                )
                api_response = response.json()

                if api_response["status"] != "success":
                    st.error("Backend error")
                    st.stop()

                st.session_state["did_result"]  = api_response["result"]
                st.session_state["did_payload"] = payload
                st.session_state.pop("placebo_result", None)

            except Exception as e:
                st.error("Connection error")
                st.write(str(e))

    if st.session_state.get("did_result"):

        result = st.session_state["did_result"]
        st.success("Estimation completed ✅")

        overall = result["overall"]
        summary_df = pd.DataFrame({
            "ATT":          [round(overall["att"], 4)],
            "Std. Error":   [round(overall["se"], 4)],
            "95% CI Lower": [round(overall["lower_ci"], 4)],
            "95% CI Upper": [round(overall["upper_ci"], 4)],
            "p-value":      [round(overall["p_value"], 4)]
        })

        st.subheader("Overall ATT Summary")
        st.dataframe(summary_df, use_container_width=True)
        st.markdown(
            "<div style='font-size:14px; margin-top:8px;'>"
            "<i>Note:</i> *** p &lt; 0.01, ** p &lt; 0.05, * p &lt; 0.10."
            "</div>",
            unsafe_allow_html=True
        )

        if result.get("pretrend_test"):
            pt = result["pretrend_test"]
            st.subheader("Joint Pre-Trend Test")
            st.write(
                f"Wald chi2({pt['df']}) = {round(pt['statistic'], 3)}, "
                f"p-value = {round(pt['p_value'], 4)}"
            )

        dynamic = result["dynamic"]
        dynamic_rows = []

        for i in range(len(dynamic["event_time"])):
            lower = dynamic["lower_ci"][i]
            upper = dynamic["upper_ci"][i]
            significant = lower > 0 or upper < 0
            dynamic_rows.append({
                "event_time": dynamic["event_time"][i],
                "ATT":        dynamic["att_egt"][i],
                "lower":      lower,
                "upper":      upper,
                "stars":      "*" if significant else ""
            })

        dynamic_df = pd.DataFrame(dynamic_rows)
        st.subheader("Dynamic ATT Estimates")
        st.dataframe(dynamic_df.round(4), use_container_width=True)

        fig, ax = plt.subplots(figsize=(7, 4))
        ax.plot(dynamic["event_time"], dynamic["att_egt"], marker="o", color="steelblue")
        ax.fill_between(
            dynamic["event_time"],
            dynamic["lower_ci"],
            dynamic["upper_ci"],
            alpha=0.2,
            color="steelblue"
        )
        ax.axhline(0, linestyle="--", color="black", linewidth=0.8)
        ax.axvline(0, linestyle=":",  color="red",   linewidth=0.8)
        ax.set_xlabel("Event Time")
        ax.set_ylabel("ATT")
        ax.set_title("Event Study (Simultaneous Bands)")
        st.pyplot(fig)
        plt.close(fig)

        st.divider()
        st.caption("Optional diagnostic: placebo tests shift treatment cohort backwards.")

        if st.button("Run Placebo Tests"):
            with st.spinner("Running placebo tests (analytic SE)..."):
                try:
                    response = requests.post(
                        "http://127.0.0.1:5000/api/placebo",
                        json=st.session_state["did_payload"],
                        timeout=300
                    )
                    api_response = response.json()
                    if api_response["status"] != "success":
                        st.error("Placebo backend error")
                    else:
                        st.session_state["placebo_result"] = api_response["result"]
                except Exception as e:
                    st.error("Connection error")
                    st.write(str(e))

        if st.session_state.get("placebo_result"):
            placebo = st.session_state["placebo_result"]
            rows = []
            for key, value in placebo.items():
                if value is not None:
                    rows.append({
                        "Shift":   -value["shift"],
                        "ATT":     value["att"],
                        "SE":      value["se"],
                        "p-value": value["p_value"],
                        "stars":   stars(value["p_value"])
                    })
            if rows:
                st.subheader("Placebo Tests")
                placebo_df = pd.DataFrame(rows)
                st.dataframe(placebo_df.round(4), use_container_width=True)
