<?php

require_once __DIR__ . '/../TemplateSampleService.php';

#[\AllowDynamicProperties]
class smpl_Invoice_Group_Sample
{
    public function getType()
    {
        return 'AOS_Invoices';
    }

    public function getBody()
    {
        global $locale;
        return '<table style="width: 100%; font-family: Arial; text-align: center;" border="0" cellpadding="2" cellspacing="2">
<tbody style="text-align: left;">
<tr style="text-align: left;">
<td style="text-align: left;">
<p><img src="'. TemplateSampleService::getAbsoluteLogoUrl() .'" style="float: left;"/>&nbsp;</p>
</td>
</tr>
<tr style="text-align: left;">
<td style="font-weight: bold; text-align: left;"><div>'.translate('LBL_BROWSER_TITLE').' Ltd<br />'.translate('LBL_ANY_STREET', 'AOS_PDF_Templates').'<br />'.translate('LBL_ANY_TOWN', 'AOS_PDF_Templates').'</span><br />'.translate('LBL_ANY_WHERE', 'AOS_PDF_Templates').'</div></td>
</tr>
<tr style="text-align: left;">
<td style="text-align: left;"></td>
</tr>
<tr style="text-align: left;">
<td style="text-align: left;">
<h1>'.strtoupper(translate('LBL_PDF_NAME', 'AOS_Invoices')).'</h1>
</td>
</tr>
</tbody>
</table>
<p style="font-family: Arial; text-align: center;">&nbsp;</p>
<table style="text-align: center; width: 100%; border: none; border-spacing: 0pt;">
<tbody style="text-align: left;">
<tr style="text-align: left;">
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;">'.translate('LBL_PREPARED_FOR', 'AOS_PDF_Templates').'</td>
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;">'.translate('LBL_PREPARED_BY', 'AOS_PDF_Templates').'</td>
</tr>
<tr style="text-align: left;">
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;">
<div>$aos_invoices_billing_account<br /> $aos_invoices_billing_address_street<br /> $aos_invoices_billing_address_city <br /> $aos_invoices_billing_address_state $aos_invoices_billing_address_postalcode</div>
<br /></td>
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;"><div>$aos_invoices_modified_by_name</div></td>
</tr>
<tr style="text-align: left;">
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;">'.translate('LBL_INVOICE_DATE', 'AOS_Invoices').'</td>
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;">'.translate('LBL_DUE_DATE', 'AOS_Invoices').'</td>
</tr>
<tr style="text-align: left;">
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;"><div>$aos_invoices_date_entered</div></td>
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;"><div>$aos_invoices_due_date</div></td>
</tr>
<tr style="text-align: left;">
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;">'.translate('LBL_INVOICE_NUMBER', 'AOS_Invoices').'</td>
<td style="font-weight: bold; background-color: #b0c4de; padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; vertical-align: top; text-align: left; width: 50%;"></td>
</tr>
<tr style="text-align: left;">
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;"><div>$aos_invoices_number</div></td>
<td style="padding: 2px 6px; border-style: solid solid solid solid;  border-width: 0.5px; width: 50%; vertical-align: top; text-align: left;"><div></div></td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
<table repeat_header="1" style="width: 100%; border: none; border-spacing: 0pt;">
<tbody>
<tr>
<td colspan="8" style="padding: 2px 6px; font-weight: bold; text-align: left;">$aos_line_item_groups_name</td>
</tr>
<tr>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 10%; font-weight: bold; text-align: center;">'.translate('LBL_PRODUCT_QUANITY', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 16%; font-weight: bold; text-align: center;">'.translate('LBL_PRODUCT_NAME', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 20%; font-weight: bold; text-align: center;">'.translate('LBL_DESCRIPTION', 'AOS_Products').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 10%; font-weight: bold; text-align: center;">'.translate('LBL_LIST_PRICE', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 11%; font-weight: bold; text-align: center;">'.translate('LBL_DISCOUNT_AMT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 10%; font-weight: bold; text-align: center;">'.translate('LBL_UNIT_PRICE', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 14%; font-weight: bold; text-align: center;">'.translate('LBL_VAT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  background-color: #b0c4de; border-width: 0.5px; padding: 2px 6px; width: 8%; font-weight: bold; text-align: center;">'.translate('LBL_TOTAL_PRICE', 'AOS_Invoices').'</td>
</tr>
<tr>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; text-align: center;">$aos_products_quotes_product_qty</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_name</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_description</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_product_list_price</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_product_discount</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_product_unit_price</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_vat</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_products_quotes_product_total_price</td>
</tr>
<tr>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;" colspan="3">$aos_services_quotes_name</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_services_quotes_service_list_price</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_services_quotes_service_discount</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_services_quotes_service_unit_price</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_services_quotes_vat</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_services_quotes_service_total_price</td>
</tr>
<tr>
<td colspan="6">&nbsp;</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; font-weight: bold; text-align: right;">'.translate('LBL_TOTAL_AMT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_line_item_groups_total_amt</td>
</tr>
<tr>
<td colspan="6">&nbsp;</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; font-weight: bold; text-align: right;">'.translate('LBL_DISCOUNT_AMOUNT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_line_item_groups_discount_amount</td>
</tr>
<tr>
<td colspan="6">&nbsp;</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; font-weight: bold; text-align: right;">'.translate('LBL_SUBTOTAL_AMOUNT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_line_item_groups_subtotal_amount</td>
</tr>
<tr>
<td colspan="6">&nbsp;</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; font-weight: bold; text-align: right;">'.translate('LBL_TAX_AMOUNT', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_line_item_groups_tax_amount</td>
</tr>
<tr>
<td colspan="6">&nbsp;</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px; font-weight: bold; text-align: right;">'.translate('LBL_GROUP_TOTAL', 'AOS_Invoices').'</td>
<td style="border-style: solid solid solid solid;  border-width: 0.5px; padding: 2px 6px;">$aos_line_item_groups_total_amount</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>';
    }

    public function getHeader()
    {
        return '';
    }

    public function getFooter()
    {
        global $locale;
        return '<table style="width: 100%; border: none; border-spacing: 0pt;">
<tbody>
<tr>
<td>'.translate('LBL_PAGE', 'AOS_PDF_Templates').' {PAGENO}</td>
<td style="text-align: right;">{DATE '.$locale->getPrecedentPreference('default_date_format').'}</td>
</tr>
</tbody>
</table>';
    }
}
