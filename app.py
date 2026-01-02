from flask import Flask, render_template, jsonify, request
import os
import json
from datetime import datetime

app = Flask(__name__)
DATA_DIR = os.path.join(os.path.dirname(__file__), "data")

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/data')
def get_data():
    date_str = request.args.get('date')
    if not date_str:
        date_str = datetime.now().strftime("%Y-%m-%d")

    day_dir = os.path.join(DATA_DIR, date_str)
    
    if not os.path.exists(day_dir):
        return jsonify({"error": "No data found for this date", "datasets": []})

    files = sorted([f for f in os.listdir(day_dir) if f.endswith(".json")])
    
    # Structure: time -> {parking_id: vacancy}
    timeline = []
    
    # We need to know all possible parking IDs to initialize datasets
    all_parking_ids = set()
    parking_details = {} # id -> description

    for filename in files:
        filepath = os.path.join(day_dir, filename)
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = json.load(f)
                
                # Extract timestamp from filename or content.
                # Filename is HH-MM-SS.json. Let's use that for simplicity as it's the capture time.
                time_part = filename.replace(".json", "").replace("-", ":")
                
                snapshot = {
                    "time": time_part,
                    "data": {}
                }

                if "data" in content and "parkings" in content["data"]:
                    parkings = content["data"]["parkings"]
                    for pid, pdata in parkings.items():
                        vacancy = pdata.get("vacancy", 0)
                        snapshot["data"][pid] = vacancy
                        all_parking_ids.add(pid)
                        if pid not in parking_details:
                            parking_details[pid] = pdata.get("description", pid)
                
                timeline.append(snapshot)

        except Exception as e:
            print(f"Error reading {filename}: {e}")
            continue

    # Format for Chart.js
    # labels: [t1, t2, t3...]
    # datasets: [{label: 'Parking A', data: [v1, v2, v3...]}, ...]
    
    labels = [t["time"] for t in timeline]
    datasets = []
    
    for pid in sorted(list(all_parking_ids)):
        data_points = []
        for point in timeline:
            # If a data point is missing for this timestamp, we can append None or 0.
            # Using None allows Chart.js to span gaps or break lines depending on config.
            data_points.append(point["data"].get(pid, None))
            
        dataset = {
            "label": parking_details.get(pid, pid),
            "data": data_points,
            "fill": False,
            "tension": 0.1
        }
        datasets.append(dataset)

    return jsonify({
        "labels": labels,
        "datasets": datasets,
        "date": date_str
    })

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)
