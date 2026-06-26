# WIT Insurance CRM Configuration

This branch adds an insurance-focused lead intake layer to SuiteCRM 7 without changing core SuiteCRM files.

## What this adds

### Lead fields

The Leads module now has fields for:

- Policy type
- Coverage limits
- Drivers, stored as structured JSON
- Vehicles and VINs, stored as structured JSON
- Accidents and violations, stored as structured JSON
- Next action
- Scheduled follow-up
- Call summary
- Missing information
- Last violation conviction date
- Violation follow-up date
- Auto follow-up required
- VIN decode status
- Source email ID
- Email parse confidence

### Driver and vehicle sections

The lead edit screen includes repeatable table-style sections for drivers, vehicles, and accidents or violations. These are stored as JSON so the VOIP project, quote intake forms, and future automation can read the same structured data.

### VIN decoding

VIN decoding is handled with the free NHTSA vPIC API. When a lead is saved, VINs in the vehicle section are decoded server-side when possible. Decoded vehicle data can include year, make, model, body class, vehicle type, manufacturer, and API error status.

### Email-to-lead parsing

A scheduler job named `WIT: Parse imported lead emails into insurance leads` reads imported emails associated with the leads mailbox and creates WIT insurance leads from the email body.

The parser currently watches for both:

- leads@weinsurethings.com
- leads@weinsruethings.com

The second address is included because it appeared in the original request. Remove it if that was only a typo.

### Call summary extraction

When fields are missing, the lead save hook tries to extract structured values from the call summary or description. It currently looks for labels such as:

- Coverage Limits:
- Next Action:
- Follow Up:
- DOB:
- Driver License:
- VIN:
- Conviction Date:

### Follow-up automation

When a scheduled follow-up date is present, a related SuiteCRM Task is created.

When a violation conviction date is present, the system calculates a violation follow-up date at conviction date plus 2 years and 11 months, then creates a related task so the agent can re-shop or follow up before the incident ages off.

## Deployment steps

1. Deploy the branch.
2. Go to Admin > Repair > Quick Repair and Rebuild.
3. Execute any generated SQL for custom fields.
4. Go to Admin > Schedulers and confirm cron is running.
5. Activate the scheduler job named `WIT: Parse imported lead emails into insurance leads`.
6. Configure the leads mailbox as a SuiteCRM inbound/group mailbox.
7. Confirm the built-in inbound email scheduler is active so SuiteCRM imports emails into the Emails module.
8. Send a test email to the leads mailbox with name, phone, email, coverage limits, VIN, driver DOB, driver license, and conviction date.
9. Confirm a Lead is created and that follow-up Tasks are created.

## Suggested email format for best parsing

Name: Jane Smith
Email: jane@example.com
Phone: 555-555-5555
Address: 123 Main St, Asheville, NC
Coverage Limits: 100/300/100
DOB: 01/15/1988
Driver License: NC1234567
VIN: 1HGCM82633A004352
Conviction Date: 04/01/2024
Next Action: Call back with quote options
Follow Up: 06/30/2026
Call Summary: Customer is looking for personal auto coverage and has one prior speeding ticket.

## Notes for the VOIP project

The structured JSON fields are intended to be easy for the phone system to write into SuiteCRM after a call. A future VOIP integration can push the call transcript into Call Summary, then let the lead save hooks fill missing fields and schedule tasks.

## Next hardening pass

Recommended next improvements:

- Add duplicate lead matching by email and phone before creating a new lead from email.
- Add an admin configuration page for mailbox aliases.
- Add stricter JSON validation messages on edit view.
- Add a dedicated Driver and Vehicle custom module if reporting on individual drivers/VINs becomes important.
- Add API endpoints for the VOIP project to create or update leads directly from call transcripts.
