<?php

require_once __DIR__ . '/../TemplateSampleService.php';

#[\AllowDynamicProperties]
class smpl_Lead_Sample
{
    public function getType()
    {
        return 'Leads';
    }
        
    public function getBody()
    {
        global $locale;
        return '<table style="width: 100%;" border="0" cellspacing="2" cellpadding="2">
<tbody style="text-align: left;">
<tr>
<td valign="top">
<p><img src="'. TemplateSampleService::getAbsoluteLogoUrl() .'" style="float: left;"/>&nbsp;</p>
</td>
<td style="font-weight: bold; text-align: right;"><div>'.translate('LBL_BROWSER_TITLE').' Ltd<br />'.translate('LBL_ANY_STREET', 'AOS_PDF_Templates').'<br />'.translate('LBL_ANY_TOWN', 'AOS_PDF_Templates').'</span><br />'.translate('LBL_ANY_WHERE', 'AOS_PDF_Templates').'</div></td>
</tr>
</tbody>
</table>
<table style="width: 100%;" border="0" cellspacing="2" cellpadding="2">
<tbody>
<tr>
<td>
<div><br /></div>
<div>$leads_name<br /></div>
<div>$leads_account_name<br /> $leads_primary_address_street<br /> $leads_primary_address_city<br /> $leads_primary_address_state<br /> $leads_primary_address_postalcode</div>
<div><br /></div>
<div>{DATE '.$locale->getPrecedentPreference('default_date_format').'}</div>
<div><br /></div>
<p>Dear $leads_first_name</p>
<p>Established in 2009, SalesAgility is a mature, cutting edge and profitable open source software consultancy focused solely on providing exceptional Customer RelationshipManagement (CRM) solutions for organisations around the world. Based in Stirling, Scotland, the company employs over 40 people and are ISO9001 and recently ISO27001 accredited.</p>
<p>SalesAgility is the driving force behind SuiteCRM. With over a decade of experience in delivering bespoke open source CRM consulting, SalesAgility was the perfect company to create a fork of the SugarCRM Community Edition when SugarCRM abandoned open source in 2013. The fork has been very successful and SuiteCRM is now acknowledged to be the world’s leading open source CRM solution.</p>
<p>At SalesAgility we strive to be the best at everything we do through valuing results, respect, accountability, openness and collaboration, and our unique culture is defined by this. Our culture and values guide every decision we make and helps SalesAgility act as a platform for change, for our clients.</p>
<p>Yours sincerely</p>
<p> </p>
<p> </p>
<p>The SalesAgility Team</p>
</td>
</tr>
</tbody>
</table>';
    }

    public function getHeader()
    {
        return '';
    }

    public function getFooter()
    {
        return '';
    }
}
