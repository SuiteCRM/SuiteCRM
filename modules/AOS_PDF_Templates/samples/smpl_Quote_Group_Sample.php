<?php

require_once __DIR__ . '/../TemplateSampleService.php';

#[\AllowDynamicProperties]
class smpl_Quote_Group_Sample
{
    public function getType()
    {
        return 'AOS_Quotes';
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
<h1 style="margin: 0; padding: 0; font-size: 26pt; font-weight: normal; letter-spacing: 1pt; color: #3b3e51;">'.strtoupper(translate('LBL_PDF_NAME', 'AOS_Quotes')).'</h1>
</td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
<table style="width: 100%; border: none; border-spacing: 0;" border="0" cellpadding="0" cellspacing="0">
<tbody>
<tr>
<td style="width: 55%; vertical-align: top; padding-right: 16px; line-height: 1.5;">$aos_quotes_billing_account<br/>$aos_quotes_billing_address_street<br/>$aos_quotes_billing_address_city<br/>$aos_quotes_billing_address_state $aos_quotes_billing_address_postalcode</td>
<td style="width: 45%; vertical-align: top; padding-left: 16px;">
<table style="width: 100%; border: none; border-spacing: 0;" border="0" cellpadding="3" cellspacing="0">
<tbody>
<tr>
<td style="border: none; width: 55%; vertical-align: top;"><strong style="color: #3b3e51;">'.translate('LBL_QUOTE_NUMBER', 'AOS_Quotes').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_quotes_number</td>
</tr>
<tr>
<td style="border: none; vertical-align: top;"><strong style="color: #3b3e51;">'.translate('LBL_QUOTE_DATE', 'AOS_Quotes').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_quotes_date_entered</td>
</tr>
<tr>
<td style="border: none; vertical-align: top;"><strong style="color: #3b3e51;">'.translate('LBL_EXPIRATION', 'AOS_Quotes').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_quotes_expiration</td>
</tr>
<tr>
<td style="border: none; vertical-align: top;"><strong style="color: #3b3e51;">'.translate('LBL_TERM', 'AOS_Quotes').'</strong></td>
<td style="border: none; vertical-align: top;">$aos_quotes_term</td>
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
<td colspan="6" style="border: 1px solid #3b3e51; background-color: #f5f3f8; padding: 6px 8px; font-size: 12pt; font-weight: bold; color: #412A79; text-align: left;">$aos_line_item_groups_name</td>
</tr>
<tr>
<td style="border-left: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; width: 30%; font-weight: bold; color: #3b3e51; text-align: left;">'.translate('LBL_DESCRIPTION', 'AOS_Products').'</td>
<td style="border-bottom: 1.5px solid #3b3e51; width: 14%; font-weight: bold; color: #3b3e51; text-align: center;">'.translate('LBL_PRODUCT_QUANITY', 'AOS_Quotes').'</td>
<td style="border-bottom: 1.5px solid #3b3e51; width: 13%; font-weight: bold; color: #3b3e51; text-align: right;">'.translate('LBL_LIST_PRICE', 'AOS_Quotes').'</td>
<td style="border-bottom: 1.5px solid #3b3e51; width: 14%; font-weight: bold; color: #3b3e51; text-align: right;">'.translate('LBL_DISCOUNT_AMT', 'AOS_Quotes').'</td>
<td style="border-bottom: 1.5px solid #3b3e51; width: 13%; font-weight: bold; color: #3b3e51; text-align: right;">'.translate('LBL_VAT', 'AOS_Quotes').'</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; width: 16%; font-weight: bold; color: #3b3e51; text-align: right;">'.translate('LBL_TOTAL_PRICE', 'AOS_Quotes').'</td>
</tr>
<tr>
<td style="border-left: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; vertical-align: top;">$aos_products_quotes_name<br/>$aos_products_description</td>
<td style="border-bottom: 1px solid #3b3e51; text-align: center; vertical-align: top;">$aos_products_quotes_product_qty</td>
<td style="border-bottom: 1px solid #3b3e51; text-align: right; vertical-align: top;">$aos_products_quotes_product_list_price</td>
<td style="border-bottom: 1px solid #3b3e51; text-align: right; vertical-align: top;">$aos_products_quotes_product_discount</td>
<td style="border-bottom: 1px solid #3b3e51; text-align: right; vertical-align: top;">$aos_products_quotes_vat</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; text-align: right; vertical-align: top;">$aos_products_quotes_product_total_price</td>
</tr>
<tr>
<td colspan="2" style="border-left: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; vertical-align: top;">$aos_services_quotes_name</td>
<td style="border-bottom: 1.5px solid #3b3e51; text-align: right; vertical-align: top;">$aos_services_quotes_service_list_price</td>
<td style="border-bottom: 1.5px solid #3b3e51; text-align: right; vertical-align: top;">$aos_services_quotes_service_discount</td>
<td style="border-bottom: 1.5px solid #3b3e51; text-align: right; vertical-align: top;">$aos_services_quotes_vat</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; text-align: right; vertical-align: top;">$aos_services_quotes_service_total_price</td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-left: 1.5px solid #3b3e51; border-top: 2px solid #3b3e51; border-bottom: 1px solid #3b3e51; text-align: right;">'.translate('LBL_SUBTOTAL_AMOUNT', 'AOS_Quotes').'</td>
<td style="border-right: 1.5px solid #3b3e51; border-top: 2px solid #3b3e51; border-bottom: 1px solid #3b3e51; text-align: right;">$aos_line_item_groups_subtotal_amount</td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-left: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; text-align: right;">'.translate('LBL_TAX_AMOUNT', 'AOS_Quotes').'</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; text-align: right;">$aos_line_item_groups_tax_amount</td>
</tr>
<tr>
<td colspan="3" style="border: none; padding: 0;"></td>
<td colspan="2" style="border-left: 1.5px solid #3b3e51; border-top: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; font-weight: bold; color: #3b3e51; text-align: right;">'.translate('LBL_GROUP_TOTAL', 'AOS_Quotes').'</td>
<td style="border-top: 1.5px solid #3b3e51; border-right: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; font-weight: bold; color: #3b3e51; text-align: right;">$aos_line_item_groups_total_amount</td>
</tr>
<tr>
<td colspan="6" style="border: none; padding: 10px 0 0 0; line-height: 0; font-size: 0;"></td>
</tr>
</tbody>
</table>
<p>&nbsp;</p>
<table style="width: 100%; border-spacing: 0;" border="0" cellpadding="8" cellspacing="0">
<tbody>
<tr>
<td colspan="2" style="border: 1px solid #3b3e51; background-color: #3b3e51; padding: 6px 8px; font-weight: bold; color: #f5f5f5; text-align: left;">'.strtoupper(translate('LBL_SUMMARY', 'AOS_PDF_Templates')).'</td>
</tr>
<tr>
<td style="border-left: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; color: #3b3e51; width: 84%; text-align: right;">'.translate('LBL_SUBTOTAL_AMOUNT', 'AOS_Quotes').'</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; color: #3b3e51; width: 16%; text-align: right;">$subtotal_amount</td>
</tr>
<tr>
<td style="border-left: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; color: #3b3e51; width: 84%; text-align: right;">'.translate('LBL_TAX_AMOUNT', 'AOS_Quotes').'</td>
<td style="border-right: 1.5px solid #3b3e51; border-bottom: 1px solid #3b3e51; color: #3b3e51; width: 16%; text-align: right;">$tax_amount</td>
</tr>
<tr>
<td style="border-left: 1.5px solid #3b3e51; border-top: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; font-weight: bold; color: #3b3e51; width: 84%; text-align: right;">'.translate('LBL_GRAND_TOTAL', 'AOS_Quotes').' $currencies_iso4217</td>
<td style="border-right: 1.5px solid #3b3e51; border-top: 1.5px solid #3b3e51; border-bottom: 1.5px solid #3b3e51; font-weight: bold; color: #3b3e51; width: 16%; text-align: right;">$total_amount</td>
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
