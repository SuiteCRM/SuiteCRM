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