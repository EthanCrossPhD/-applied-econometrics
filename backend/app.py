from flask import Flask, request, jsonify
import subprocess
import json
import tempfile
import os

app = Flask(__name__)

def run_r_script(script_name, payload):
    script_path = os.path.join(os.path.dirname(__file__), script_name)

    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False, encoding='utf-8') as f:
        json.dump(payload, f)
        tmp_path = f.name

    try:
        result = subprocess.run(
            ['Rscript', script_path, tmp_path],
            capture_output=True,
            text=True,
            timeout=600
        )

        print("=== STDOUT ===")
        print(result.stdout[:500])
        print("=== STDERR ===")
        print(result.stderr[:2000])

        if result.returncode != 0:
            return None, result.stderr
        output = json.loads(result.stdout)
        return output, None
    except subprocess.TimeoutExpired:
        return None, "R script timed out"
    except Exception as e:
        return None, str(e)
    finally:
        try:
            os.unlink(tmp_path)
        except:
            pass


@app.route('/api/did', methods=['POST'])
def run_did():
    try:
        payload = request.json
        result, error = run_r_script('run_did_api.R', payload)
        if error:
            return jsonify({"status": "error", "message": error}), 500
        return jsonify({"status": "success", "result": result})
    except Exception as e:
        return jsonify({"status": "error", "message": str(e)}), 500


@app.route('/api/placebo', methods=['POST'])
def run_placebo():
    try:
        payload = request.json
        result, error = run_r_script('run_placebo_api.R', payload)
        if error:
            return jsonify({"status": "error", "message": error}), 500
        return jsonify({"status": "success", "result": result})
    except Exception as e:
        return jsonify({"status": "error", "message": str(e)}), 500


if __name__ == '__main__':
    app.run(debug=True)