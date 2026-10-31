// =====================================================
// Difference-in-Differences frontend logic (v1)
// Role: orchestrator, NOT estimator
// =====================================================

function computeDID() {
  const Y_T_pre  = parseFloat(document.getElementById("Y_T_pre").value);
  const Y_T_post = parseFloat(document.getElementById("Y_T_post").value);
  const Y_C_pre  = parseFloat(document.getElementById("Y_C_pre").value);
  const Y_C_post = parseFloat(document.getElementById("Y_C_post").value);

  const resultBox = document.getElementById("result");

  // Basic validation
  if ([Y_T_pre, Y_T_post, Y_C_pre, Y_C_post].some(v => isNaN(v))) {
    resultBox.innerHTML =
      "<span style=\"color:darkred;\">Please enter all four numeric values.</span>";
    return;
  }

  const payload = {
    Y_T_pre:  Y_T_pre,
    Y_T_post: Y_T_post,
    Y_C_pre:  Y_C_pre,
    Y_C_post: Y_C_post
  };

  fetch("http://127.0.0.1:5000/api/did", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload)
  })
  .then(response => {
    if (!response.ok) {
      throw new Error("Backend error");
    }
    return response.json();
  })
  .then(data => {
    resultBox.innerHTML =
      "<div><strong>ATT = " + data.ATT + "</strong></div>" +
      "<div style=\"margin-top:8px; font-size:0.9em; color:#555;\">" +
      "Interpretation: This is the Difference-in-Differences estimate. " +
      "Validity requires the <em>parallel trends</em> assumption." +
      "</div>";
  })
  .catch(() => {
    resultBox.innerHTML =
      "<span style=\"color:darkred;\">Backend not reachable.</span>";
  });
}

