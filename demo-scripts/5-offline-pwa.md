# Demo Script 5: Offline Progressive Web App (PWA)
**Duration: 2-3 minutes**

## Opening Hook (10 seconds)
"No signal at the showing? No problem. Watch how agents stay productive anywhere, anytime."

## Setup (15 seconds)
1. Show mobile device with SuiteCRM
2. Display network status: "✓ Online - LTE"
3. Say: "Let's see what happens when we lose connection"
4. Enable airplane mode

## Feature Demonstration (90 seconds)

### 1. Seamless Offline Transition (20 seconds)
- Toggle airplane mode ON
- Show status change: "📱 Offline Mode"
- Try loading property list - works instantly
- Say: "No spinning wheels, no errors, just works"
- Navigate between screens smoothly

### 2. Full Functionality Offline (30 seconds)
- Create new property listing:
  - Add photos from gallery
  - Enter details
  - Set price: $525,000
- Add client contact:
  - Name: "Sarah Johnson"
  - Phone: saved locally
- Schedule showing:
  - Tomorrow 2 PM
  - Add notes
- Show queue indicator: "3 items pending sync"
- Say: "Everything saves locally, encrypted"

### 3. Smart Sync When Reconnected (25 seconds)
- Toggle airplane mode OFF
- Watch sync process:
  ```
  🔄 Syncing...
  ✓ Property uploaded (1/3)
  ✓ Contact created (2/3)
  ✓ Showing scheduled (3/3)
  Sync complete: 2.1 seconds
  ```
- Show conflict resolution:
  - "Price updated by office: $520,000"
  - "Your version: $525,000"
  - "Resolution: Keep latest"
- Highlight: "Intelligent sync, no data loss"

### 4. PWA Installation (15 seconds)
- Show "Add to Home Screen" prompt
- Install as app
- Open from home screen - full screen
- Show app features:
  - Push notifications
  - Camera access
  - GPS working
- Say: "Feels native, works everywhere"

## Results & Benefits (20 seconds)
- Show offline usage stats:
  - "2,847 offline sessions this month"
  - "14,293 items synced successfully"
  - "0 data conflicts unresolved"
- Display testimonial: "Saved 3 deals during network outages"
- Highlight: "Never lose a lead due to connectivity"

## Call to Action (10 seconds)
"Keep selling, even offline. Get the PWA advantage at [website]"

---

## Technical Notes for Demo:
- Access via: `https://localhost:8082` (PWA requires HTTPS)
- Use Chrome DevTools to simulate offline
- Have demo data pre-cached
- Show service worker in action
- Test on actual mobile device if possible