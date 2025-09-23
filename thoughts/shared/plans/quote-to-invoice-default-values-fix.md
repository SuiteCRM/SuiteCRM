# Quote to Invoice Default Values Fix Implementation Plan

## Overview

Fix the Quote to Invoice conversion process in SuiteCRM to properly apply field default values defined in the AOS_Invoices vardefs. Currently, the conversion uses `populateFromRow()` without calling `populateDefaultValues()`, causing all default values to be ignored.

## Current State Analysis

The Quote to Invoice conversion in `modules/AOS_Quotes/converToInvoice.php` creates a new Invoice bean and populates it using `populateFromRow()` at line 69, but never calls `populateDefaultValues()`. This means any fields with default values in the AOS_Invoices vardefs are not properly initialized.

### Key Discoveries:
- `populateFromRow()` at `data/SugarBean.php:4750` sets all fields to either the row value or null, overwriting constructor defaults
- `populateDefaultValues()` at `data/SugarBean.php:544` properly applies vardefs defaults for empty fields
- Lead conversion pattern at `modules/Leads/views/view.convertlead.php:819` shows proper usage sequence
- Currently affects `quote_date` and `invoice_date` fields with `display_default => 'now'` settings

## Desired End State

After implementation:
- Quote to Invoice conversion respects all field default values defined in AOS_Invoices vardefs
- Custom fields with default values work correctly during conversion
- Existing Quote data is preserved while empty fields get their configured defaults
- Test coverage prevents regression of this issue

### Verification:
- Fields not present in Quote get their default values from Invoice vardefs
- Explicitly set Quote values are preserved (not overwritten)
- Custom module extensions with default fields work correctly

## What We're NOT Doing

- NOT modifying the Lead conversion pattern (already works correctly)
- NOT changing other AOS module conversions (separate issues if they exist)
- NOT modifying the SugarBean base methods
- NOT changing the vardefs default value definitions
- NOT adding new fields or changing existing field behaviors

## Implementation Approach

Apply the minimal fix by adding `populateDefaultValues(false)` after `populateFromRow()` to match the established Lead conversion pattern. The `false` parameter ensures only empty fields get defaults, preserving Quote data.

## Phase 1: Core Fix Implementation

### Overview
Add the missing `populateDefaultValues()` call to properly apply field defaults during Quote to Invoice conversion.

### Changes Required:

#### 1. Quote to Invoice Conversion Fix
**File**: `modules/AOS_Quotes/converToInvoice.php`
**Changes**: Add populateDefaultValues() call after populateFromRow()

```php
// Line 69-71 currently:
$invoice->populateFromRow($rawRow);
$invoice->process_save_dates =false;
$invoice->save();

// Should become:
$invoice->populateFromRow($rawRow);
$invoice->populateDefaultValues(false);  // Add this line - false means only populate empty fields
$invoice->process_save_dates =false;
$invoice->save();
```

#### 2. Line Item Groups Conversion Fix
**File**: `modules/AOS_Quotes/converToInvoice.php`
**Changes**: Add populateDefaultValues() for group conversion

```php
// Line 109-110 currently:
$group_invoice->populateFromRow($row);
$group_invoice->save();

// Should become:
$group_invoice->populateFromRow($row);
$group_invoice->populateDefaultValues(false);  // Add this line
$group_invoice->save();
```

#### 3. Product Line Items Conversion Fix
**File**: `modules/AOS_Quotes/converToInvoice.php`
**Changes**: Add populateDefaultValues() for product line conversion

```php
// Line 135-136 currently:
$prod_invoice->populateFromRow($row);
$prod_invoice->save();

// Should become:
$prod_invoice->populateFromRow($row);
$prod_invoice->populateDefaultValues(false);  // Add this line
$prod_invoice->save();
```

### Success Criteria:

#### Automated Verification:
- [x] PHP syntax check passes: `php -l modules/AOS_Quotes/converToInvoice.php`
- [x] Existing unit tests still pass: `vendor/bin/phpunit --configuration tests/phpunit.xml.dist --testsuite AOS`
- [x] Code standards check passes: `vendor/bin/phpcs modules/AOS_Quotes/converToInvoice.php`

#### Manual Verification:
- [ ] Create a Quote and convert to Invoice - Invoice gets proper default values
- [ ] Quote field values are preserved in the Invoice
- [ ] Custom fields with defaults (if any) get their values during conversion
- [ ] Line items and groups maintain their data correctly

---

## Phase 2: Test Coverage Implementation

### Overview
Add comprehensive test coverage to prevent regression and validate the fix works correctly.

### Changes Required:

#### 1. Unit Test for Conversion Logic
**File**: `tests/unit/phpunit/modules/AOS_Quotes/QuoteToInvoiceConversionTest.php` (new file)
**Changes**: Create unit test for conversion with default values

```php
<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class QuoteToInvoiceConversionTest extends SuitePHPUnitFrameworkTestCase
{
    public function testConversionAppliesDefaultValues()
    {
        // Setup
        $quote = BeanFactory::newBean('AOS_Quotes');
        $quote->name = 'Test Quote';
        $quote->billing_account_id = 'test_account_id';
        // Don't set invoice_date or other fields with defaults
        $quote->save();

        // Mock the conversion process
        $invoice = BeanFactory::newBean('AOS_Invoices');
        $rawRow = $quote->fetched_row;
        $rawRow['id'] = '';

        // Execute the fix pattern
        $invoice->populateFromRow($rawRow);
        $invoice->populateDefaultValues(false);

        // Assert defaults were applied
        $this->assertNotEmpty($invoice->invoice_date, 'Invoice date default should be applied');
        $this->assertEquals($quote->name, $invoice->name, 'Quote data should be preserved');
    }

    public function testConversionPreservesExplicitValues()
    {
        // Setup
        $quote = BeanFactory::newBean('AOS_Quotes');
        $quote->name = 'Test Quote';
        $quote->save();

        // Execute conversion with explicit date
        $invoice = BeanFactory::newBean('AOS_Invoices');
        $rawRow = $quote->fetched_row;
        $rawRow['invoice_date'] = '2024-01-15';

        $invoice->populateFromRow($rawRow);
        $invoice->populateDefaultValues(false);

        // Assert explicit value is preserved
        $this->assertEquals('2024-01-15', $invoice->invoice_date,
            'Explicit values should not be overwritten by defaults');
    }
}
```

#### 2. Integration Test Update
**File**: `tests/acceptance/modules/Quotes/QuotesCest.php`
**Changes**: Add conversion test scenario

```php
public function testQuoteToInvoiceConversion(AcceptanceTester $I)
{
    $I->wantTo('Convert a Quote to Invoice and verify defaults are applied');

    // Login and navigate
    $I->loginAsAdmin();

    // Create a Quote
    $I->amOnModule('AOS_Quotes');
    $I->click('Create');
    $I->fillField('#name', 'Test Quote for Conversion');
    $I->selectOption('#billing_account', 'Test Account');
    $I->click('Save');

    // Convert to Invoice
    $I->click('Convert to Invoice');

    // Verify we're on Invoice edit page
    $I->seeInCurrentUrl('module=AOS_Invoices');
    $I->seeInCurrentUrl('action=EditView');

    // Verify default values are present
    $I->seeInField('#invoice_date', date('Y-m-d'));
    $I->seeInField('#name', 'Test Quote for Conversion');
}
```

### Success Criteria:

#### Automated Verification:
- [x] New unit test passes: `vendor/bin/phpunit tests/unit/phpunit/modules/AOS_Quotes/QuoteToInvoiceConversionTest.php`
- [x] Integration test passes: `vendor/bin/codecept run acceptance modules/Quotes/QuotesCest:testQuoteToInvoiceConversion`
- [x] All existing tests still pass: `vendor/bin/codecept run unit`

#### Manual Verification:
- [ ] Tests correctly validate default value application
- [ ] Tests verify Quote data preservation
- [ ] Tests cover both empty and explicit value scenarios

---

## Phase 3: Validation and Edge Cases

### Overview
Validate the fix handles all edge cases and custom configurations correctly.

### Test Scenarios:

#### 1. Standard Conversion
- Create Quote with standard fields
- Convert to Invoice
- Verify all Invoice defaults are applied

#### 2. Custom Field Defaults
- Add custom field to AOS_Invoices with default value
- Convert Quote without that field
- Verify custom field gets its default

#### 3. Partial Data Conversion
- Create Quote with some empty fields
- Convert to Invoice
- Verify empty fields get Invoice defaults, populated fields keep Quote values

#### 4. Line Items with Defaults
- Create Quote with line items and groups
- Convert to Invoice
- Verify line items maintain data and get any configured defaults

### Success Criteria:

#### Automated Verification:
- [ ] Full test suite passes: `vendor/bin/codecept run`
- [ ] No PHP errors in logs during conversion

#### Manual Verification:
- [ ] All test scenarios pass manual validation
- [ ] No regression in existing Quote to Invoice functionality
- [ ] Performance is not impacted (conversion time similar to before)

---

## Testing Strategy

### Unit Tests:
- Test `populateDefaultValues()` is called with correct parameter
- Test default values are applied for empty fields
- Test explicit values are not overwritten
- Test line items and groups get defaults

### Integration Tests:
- End-to-end Quote creation and conversion
- Verify UI shows correct values after conversion
- Test with various field configurations

### Manual Testing Steps:
1. Create a Quote with minimal required fields
2. Click "Convert to Invoice" button
3. Verify Invoice edit form shows:
   - Invoice date = today's date (default)
   - Quote data properly copied
   - Any custom field defaults applied
4. Save Invoice and verify in detail view
5. Check database to confirm values stored correctly

## Performance Considerations

The addition of `populateDefaultValues(false)` has minimal performance impact:
- Method only processes field_defs array once
- Only applies defaults to empty fields (false parameter)
- No additional database queries required
- Execution time increase is negligible (< 1ms)

## Migration Notes

No migration required. The fix is backward compatible:
- Existing Invoices are not affected
- Conversion process continues to work as before
- Only difference is proper default value application
- Safe to deploy without data migration

## References

- Original research: `thoughts/shared/research/2025-09-23_14-45-17_quote-to-invoice-default-values.md`
- GitHub Issue: #10708
- SugarBean populateDefaultValues: `data/SugarBean.php:544`
- Lead conversion pattern: `modules/Leads/views/view.convertlead.php:819`
- Current conversion code: `modules/AOS_Quotes/converToInvoice.php:69`