# Demo Script 3: Real-Time Mainframe Synchronization
**Duration: 2-3 minutes**

## Opening Hook (10 seconds)
"Your MLS runs on a 30-year-old mainframe. Your agents use iPhones. Watch them work together seamlessly."

## Setup (15 seconds)
1. Show split screen: Legacy MLS terminal (left), SuiteCRM (right)
2. Display connection status: "Mainframe: IBM z/OS Connected"
3. Say: "No more double entry. No more outdated listings."

## Feature Demonstration (90 seconds)

### 1. Instant MLS Sync (30 seconds)
- Click "Sync from Mainframe"
- Show COBOL processing:
  ```
  MAINFRAME-SYNC STARTING
  CONNECTING TO MLS-SYSTEM
  RECORDS FOUND: 1,247
  PROCESSING BATCH 1 OF 5
  ```
- Watch properties populate in real-time
- Highlight: "1,247 listings synced in 8 seconds"

### 2. Bi-Directional Updates (25 seconds)
- Update price in SuiteCRM: $450,000 → $425,000
- Show instant mainframe update:
  ```
  PROPERTY-UPDATE RECEIVED
  MLS-ID: 2024-78432
  FIELD: PRICE
  UPDATE: CONFIRMED
  ```
- Legacy terminal shows new price
- Say: "Changes flow both ways, instantly"

### 3. Agent Data Sync (20 seconds)
- Add new agent in SuiteCRM
- Watch COBOL process:
  ```
  AGENT-PROFILE CREATE
  LICENSE: RE-2024-9847
  MLS-ACCESS: GRANTED
  COMMISSION-SPLIT: SET
  ```
- Show agent appearing in mainframe
- Highlight: "New agents productive in minutes, not days"

### 4. Conflict Resolution (15 seconds)
- Demo conflicting update
- Show smart resolution:
  ```
  CONFLICT DETECTED
  MAINFRAME: $425,000 (14:32:01)
  CRM: $420,000 (14:32:15)
  RESOLUTION: MOST RECENT
  ```
- Say: "Intelligent conflict handling prevents data loss"

## Results & Benefits (20 seconds)
- Show sync dashboard:
  - "Last sync: 2 seconds ago"
  - "Records synced today: 45,892"
  - "Sync errors: 0"
- Display time saved: "4 hours/day per office"
- Highlight: "Zero data discrepancies in 6 months"

## Call to Action (10 seconds)
"Connect your legacy systems to modern CRM. Visit [website] for a custom integration demo."

---

## Technical Notes for Demo:
- Use MAINFRAME.cob for sync simulation
- Access via WebSocket port 8081
- Show both green-screen terminal and modern UI
- Have sample MLS data ready
- Demonstrate real-time WebSocket updates