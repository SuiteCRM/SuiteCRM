<?php

require_once __DIR__ . '/../TemplateSampleService.php';

#[\AllowDynamicProperties]
class smpl_Account_Sample
{
    public function getType()
    {
        return 'Accounts';
    }

    public function getBody()
    {
        global $locale;
        return '<table style="width: 100%; font-family: DejaVu Sans;" border="0" cellspacing="2" cellpadding="2">
<tbody style="text-align: left;">
<tr>
<td valign="top">
<p><img src="'. TemplateSampleService::getAbsoluteLogoUrl() .'" style="float: left;"/>&nbsp;</p>
</td>
<td style="text-align: right;"><div>'.translate('LBL_BROWSER_TITLE').'<br />'.translate('LBL_ANY_STREET', 'AOS_PDF_Templates').'<br />'.translate('LBL_ANY_TOWN', 'AOS_PDF_Templates').'<br />'.translate('LBL_ANY_WHERE', 'AOS_PDF_Templates').'</div></td>
</tr>
</tbody>
</table>
<div><br /></div>
<div>$accounts_name<br /> $accounts_billing_address_street<br /> $accounts_billing_address_city<br /> $accounts_billing_address_state<br /> $accounts_billing_address_postalcode</div>
<div><br /></div>
<div>{DATE '.$locale->getPrecedentPreference('default_date_format').'}</div>
<div><br /></div>
<p>Dear $accounts_name</p>
<p>We believe organisations should have full control over the software they use and the data they hold. Open source is what makes that possible: when the code is open, you can see exactly how your CRM works and adapt it to the way your organisation operates. It&#39;s an approach rooted in transparency, collaboration and innovation, the values that guide everything we do.</p>
<p>We are passionate about open source and it sits at the heart of our work. We provide software that is free to download and code that is open to everyone, built on years of open collaboration and improved continuously through contributions from the community.</p>
<p>SuiteCRM started in 2013 as a fork of SugarCRM&#39;s Community Edition and has since grown into one of the most capable open source CRMs available. At SuiteCRM Ltd, we continue to lead its development, so that organisations around the world can run their CRM on their own terms, with genuine ownership of their software and their data.</p>
<p>Yours sincerely</p>
<p> </p>
<p> </p>
<p>The SuiteCRM Team</p>';
    }

    public function getHeader()
    {
        return '';
    }

    public function getFooter()
    {
        global $locale;
        return '<table border="0" style="width: 100%; border: none; border-collapse: collapse; border-spacing: 0pt;">
<tbody>
<tr>
<td style="border: none;">'.translate('LBL_PAGE', 'AOS_PDF_Templates').' {PAGENO}</td>
<td style="border: none; text-align: right;">{DATE '.$locale->getPrecedentPreference('default_date_format').'}</td>
</tr>
</tbody>
</table>';
    }
}
