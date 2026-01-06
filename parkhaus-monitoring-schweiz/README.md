# Swiss Parking Availability Monitor (Parkhaus Monitoring Schweiz)

**[📱 OPEN LIVE DASHBOARD](https://roger-infanger-weibel.github.io/parkhaus-monitoring-schweiz/)**

This project is an automated tool designed to fetch and store real-time parking availability data from multiple Swiss cities with PLS (Parkleitsystem) parking guidance systems. It monitors parking availability across **Luzern, Basel, St. Gallen, and Zürich**.

## Features

- **Multi-City Monitoring**: Tracks parking availability in 4 major Swiss cities
- **Real-time Data Fetching**: Retrieves parking data from official city APIs
- **Automated Archiving**: GitHub Actions workflow runs every 15 minutes
- **Structured Storage**: Data organized by city and date (`data/{city}/YYYY-MM-DD/HH-MM-SS.json`)
- **Interactive Dashboard**: Web-based visualization with city and date selection

## Monitored Cities

| City | Facilities | API Source |
|------|-----------|------------|
| **Luzern** | 15 facilities (~4,000 spaces) | [pls-luzern.ch](https://info.pls-luzern.ch/TeqParkingWS/GetFreeParks) |
| **Basel** | 16 facilities (~5,000 spaces) | [ParkenDD API](https://api.parkendd.de/Basel) |
| **St. Gallen** | 16 facilities (~2,200 spaces) | [St. Gallen Open Data](https://daten.stadt.sg.ch) |
| **Zürich** | 36 facilities (~9,000+ spaces) | [ParkenDD API](https://api.parkendd.de/Zuerich) |

**Total**: 83+ parking facilities with over 20,000 parking spaces monitored.

## Project Structure

```
parkhaus-monitoring-schweiz/
├── collectors/              # City-specific data collectors
│   ├── __init__.py
│   ├── base.py             # Base collector class
│   ├── luzern.py           # Luzern collector
│   ├── basel.py            # Basel collector
│   ├── stgallen.py         # St. Gallen collector
│   └── zurich.py           # Zürich collector
├── config/
│   └── cities.json         # City configuration
├── data/                   # Collected data (organized by city)
│   ├── luzern/
│   ├── basel/
│   ├── stgallen/
│   └── zurich/
├── .github/workflows/
│   └── scrape.yml          # GitHub Actions automation
├── collect_data.py         # Main data collection orchestrator
├── index.html              # Interactive web dashboard
├── requirements.txt        # Python dependencies
└── README.md
```

## Setup

### 1. Clone the repository
```bash
git clone https://github.com/roger-infanger-weibel/parkhaus-monitoring-schweiz.git
cd parkhaus-monitoring-schweiz
```

### 2. Install dependencies
Ensure you have Python 3.8+ installed, then run:
```bash
pip install -r requirements.txt
```

## Usage

### Manual Data Collection

**Collect data for all cities:**
```bash
python collect_data.py --once
```

**Collect data for a specific city:**
```bash
python collect_data.py --city luzern --once
python collect_data.py --city basel --once
python collect_data.py --city stgallen --once
python collect_data.py --city zurich --once
```

### Continuous Monitoring (Local)

Run the script continuously with a 15-minute interval:
```bash
python collect_data.py

# Or with a custom interval (e.g., 60 seconds for testing)
python collect_data.py --interval 60
```

## Automation

The project uses GitHub Actions to run automatically:
- **Schedule**: Every 15 minutes
- **Workflow**: Checks out code, installs dependencies, runs `collect_data.py --once`, and commits new data files

## Data Format

Each city's data is stored in a unified JSON format:

```json
{
  "status": "success",
  "city": "luzern",
  "data": {
    "parkings": {
      "PARKING_ID": {
        "id": "PARKING_ID",
        "name": "Parking Name",
        "free": 150,
        "total": 200,
        "status": "open",
        "timestamp": "2026-01-06T07:30:00+01:00"
      }
    }
  },
  "timestamp": "2026-01-06T07:30:00+01:00"
}
```

## Configuration

City configurations are stored in `config/cities.json`. To enable/disable a city:

```json
{
  "cities": {
    "luzern": {
      "enabled": true,
      "name": "Luzern",
      ...
    }
  }
}
```

## Web Dashboard

The interactive dashboard (`index.html`) provides:
- City selector dropdown
- Date selector for historical data
- Real-time Chart.js visualization
- Parking facility trends over time

Access the live dashboard at: https://roger-infanger-weibel.github.io/parkhaus-monitoring-schweiz/

## Development

### Adding a New City

1. Create a new collector in `collectors/{city}.py` extending `BaseParkingCollector`
2. Add city configuration to `config/cities.json`
3. Test with `python collect_data.py --city {city} --once`

### Running Tests

```bash
# Test all collectors
python -m pytest tests/

# Test specific city
python collect_data.py --city luzern --once
```

## Data Sources

- **Luzern**: https://info.pls-luzern.ch/TeqParkingWS/GetFreeParks
- **Basel**: https://api.parkendd.de/Basel
- **St. Gallen**: https://daten.stadt.sg.ch/api/records/1.0/search/?dataset=freie-parkplatze-in-der-stadt-stgallen-pls
- **Zürich**: https://api.parkendd.de/Zuerich

## License

MIT License - Feel free to use and modify for your own projects.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Acknowledgments

- City of Luzern for providing the PLS API
- City of Basel for open parking data
- City of St. Gallen for open data initiative
- City of Zürich for open data portal
- ParkenDD project for API aggregation
