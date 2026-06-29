<?php

require_once __DIR__ . '/../TemplateSampleService.php';

#[\AllowDynamicProperties]
class smpl_Invoice_Sample
{
    public function getType()
    {
        return 'AOS_Invoices';
    }

    public function getBody()
    {
        global $locale;
        return '<div style="font-family: DejaVu Sans;">
<table style="width: 100%; border: none; border-spacing: 0;" border="0" cellpadding="0" cellspacing="0">
<tbody>
<tr>
<td style="width: 55%; vertical-align: bottom; padding-bottom: 10px; line-height: 1.5;">
<div style="margin-top: 6px;">'.translate('LBL_BROWSER_TITLE').'<br/>'.translate('LBL_ANY_STREET', 'AOS_PDF_Templates').'<br/>'.translate('LBL_ANY_TOWN', 'AOS_PDF_Templates').'<br/>'.translate('LBL_ANY_WHERE', 'AOS_PDF_Templates').'</div>
</td>
<td style="width: 45%; text-align: right; vertical-align: top;">
<div><img src="'. TemplateSampleService::getAbsoluteLogoUrl() .'" style="max-height: 60px;"/></div>
</td>
</tr>
<tr>
<td colspan="2" vertical-align: top;">
<p>&nbsp;</p>
<h1 style="margin: 0; padding: 0; font-size: 26pt; font-weight: normal; letter-spacing: 1pt;">'.strtoupper(translate('LBL_PDF_NAME', 'AOS_Invoices')).'</h1>
</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
<table style="width: 100%; border: none; border-spacing: 0;" border="0" cellpadding="0" cellspacing="0">
<tbody>
<tr>
<td style="width: 55%; vertical-align: top; padding-right: 16px; line-height: 1.5;">$aos_invoices_billing_account<br/>$aos_invoices_billing_address_street<br/>$aos_invoices_billing_address_city<br/>$aos_invoices_billing_address_state $aos_invoices_billing_address_postalcode</td>
<td style="width: 45%; vertical-align: top; padding-left: 16px;">
<table style="width: 100%; border: none; border-spacing: 0;" border="0" cellpadding="3" cellspacing="0">
<tbody>
<tr>
<td style="border: none; width: 55%; vertical-align: top;"><strong style="color: #333333;">'.translate('LBL_INVOICE_NUMBER', 'AOS_Invoices').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_invoices_number</td>
</tr>
<tr>
<td style="border: none; vertical-align: top;"><strong style="color: #333333;">'.translate('LBL_INVOICE_DATE', 'AOS_Invoices').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_invoices_invoice_date</td>
</tr>
<tr>
<td style="border: none; vertical-align: top;"><strong style="color: #333333;">'.translate('LBL_DUE_DATE', 'AOS_Invoices').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_invoices_due_date</td>
</tr>
</tbody>
</table>
</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
<table style="width: 100%; border-spacing: 0;" border="0" cellpadding="8" cellspacing="0">
<tbody>
<tr>
<td style="border-bottom: 1.5px solid #333333; width: 30%; font-weight: bold; color: #333333; text-align: left;">'.translate('LBL_DESCRIPTION', 'AOS_Products').'</td>
<td style="border-bottom: 1.5px solid #333333; width: 14%; font-weight: bold; color: #333333; text-align: center;">'.translate('LBL_PRODUCT_QUANITY', 'AOS_Invoices').'</td>
<td style="border-bottom: 1.5px solid #333333; width: 13%; font-weight: bold; color: #333333; text-align: right;">'.translate('LBL_LIST_PRICE', 'AOS_Invoices').'</td>
<td style="border-bottom: 1.5px solid #333333; width: 14%; font-weight: bold; color: #333333; text-align: right;">'.translate('LBL_DISCOUNT_AMT', 'AOS_Invoices').'</td>
<td style="border-bottom: 1.5px solid #333333; width: 13%; font-weight: bold; color: #333333; text-align: right;">'.translate('LBL_VAT', 'AOS_Invoices').'</td>
<td style="border-bottom: 1.5px solid #333333; width: 16%; font-weight: bold; color: #333333; text-align: right;">'.translate('LBL_TOTAL_PRICE', 'AOS_Invoices').'</td>
</tr>
<tr>
<td style="border-bottom: 1px solid #dddddd; vertical-align: top;">$aos_products_quotes_name<br/>$aos_products_description</td>
<td style="border-bottom: 1px solid #dddddd; text-align: center; vertical-align: top;">$aos_products_quotes_product_qty</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_products_quotes_product_list_price</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_products_quotes_product_discount</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_products_quotes_vat</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_products_quotes_product_total_price</td>
</tr>
<tr>
<td colspan="2" style="border-bottom: 1px solid #dddddd; vertical-align: top;">$aos_services_quotes_name</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_services_quotes_service_list_price</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_services_quotes_service_discount</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_services_quotes_vat</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right; vertical-align: top;">$aos_services_quotes_service_total_price</td>
</tr>
<tr>
<td colspan="6" style="border-top: 2px solid #333333; padding: 0; line-height: 0; font-size: 0;"></td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-bottom: 1px solid #dddddd; text-align: right;">'.translate('LBL_SUBTOTAL_AMOUNT', 'AOS_Invoices').'</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right;">$subtotal_amount</td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-bottom: 1px solid #dddddd; text-align: right;">'.translate('LBL_TAX_AMOUNT', 'AOS_Invoices').'</td>
<td style="border-bottom: 1px solid #dddddd; text-align: right;">$tax_amount</td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-top: 1.5px solid #333333; border-bottom: 1.5px solid #333333; font-weight: bold; text-align: right;">'.translate('LBL_GRAND_TOTAL', 'AOS_Invoices').' $currencies_iso4217</td>
<td style="border-top: 1.5px solid #333333; border-bottom: 1.5px solid #333333; font-weight: bold; text-align: right;">$total_amount</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
</div>';
    }

    public function getHeader()
    {
        return '';
    }

    public function getFooter()
    {
        return '<table border="0" style="width: 100%; border: none; border-collapse: collapse; border-spacing: 0pt;">
<tbody>
<tr>
<td style="padding-top: 14px; padding-bottom: 8px; text-align: center; font-size: 9pt; color: #666666;">'.translate('LBL_BROWSER_TITLE').'&nbsp;&middot;&nbsp;'.translate('LBL_COMPANY_REG_NO', 'AOS_PDF_Templates').' 12345678&nbsp;&middot;&nbsp;'.translate('LBL_REGISTERED_OFFICE', 'AOS_PDF_Templates').': '.translate('LBL_ANY_STREET', 'AOS_PDF_Templates').', '.translate('LBL_ANY_TOWN', 'AOS_PDF_Templates').', '.translate('LBL_ANY_WHERE', 'AOS_PDF_Templates').'</td>
</tr>
</tbody>
</table>';
    }
}
