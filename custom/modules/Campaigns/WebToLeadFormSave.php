<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once 'include/formbase.php';
require_once 'include/SugarTinyMCE.php';

global $mod_strings;
global $app_strings;
//-----------begin replacing text input tags that have been marked with text area tags
//get array of text areas strings to process
$bodyHTML = html_entity_decode($_REQUEST['body_html'], ENT_QUOTES);

while (strpos($bodyHTML, "ta_replace") !== false) {

    //define the marker edges of the sub string to process (opening and closing tag brackets)
    $marker = strpos($bodyHTML, "ta_replace");
    $start_border = strpos($bodyHTML, "input", $marker) - 1; // to account for opening '<' char;
    $end_border = strpos($bodyHTML, '>', $start_border); //get the closing tag after marker ">";

    //extract the input tag string
    $working_str = substr($bodyHTML, $marker - 3, $end_border - ($marker - 3));

    //replace input markup with text areas markups
    $new_str = str_replace('input', 'textarea', $working_str);
    $new_str = str_replace("type=\"text\"", ' ', $new_str);
    $new_str = $new_str . '> </textarea';

    //replace the marker with generic term
    $new_str = str_replace('ta_replace', 'sugarslot', $new_str);

    // NET-enabling start-tag requires SHORTTAG YES
    $new_str = str_replace('/> </textarea>', '> </textarea>', $new_str);

    //merge the processed string back into bodyhtml string
    $bodyHTML = str_replace($working_str, $new_str, $bodyHTML);
}
//<<<----------end replacing marked text inputs with text area tags

$guid = create_guid();
$form_file = "upload://$guid";
$SugarTiny = new SugarTinyMCE();
$html = $SugarTiny->cleanEncodedMCEHtml($bodyHTML);

// STIC-Custom 20230823 JBL - Add reCAPTCHA validation to forms
// STIC#1180
$includeRecaptcha = intval($_REQUEST['recaptcha_checked']);
$recaptchaName = $_REQUEST['recaptcha_name'];
$recaptchaKey = $_REQUEST['recaptcha_key'];
$recaptchaWebKey = $_REQUEST['recaptcha_webkey'];
$recaptchaVersion = intval($_REQUEST['recaptcha_version']);
$redirectUrlKoRecaptcha = $_REQUEST['redirect_url_ko_recaptcha'];

if ($includeRecaptcha==1) {
    if ($recaptchaVersion==2) {
        $html.='<script src="https://www.google.com/recaptcha/api.js" async defer></script>';
        $html = str_replace("</form>", '<div class="g-recaptcha" data-sitekey="'.$recaptchaWebKey.'"></div></form>', $html);
    }
    if ($recaptchaVersion==3) {
        $html.='<script src="https://www.google.com/recaptcha/api.js?render='.$recaptchaWebKey.'"></script>';
    }
    if ($redirectUrlKoRecaptcha!='https://') {
        $html = str_replace("</form>", '<input type="hidden" id="redirect_ko_url" name="redirect_ko_url" value="'.$redirectUrlKoRecaptcha.'" /></form>', $html);
    }
    if ($recaptchaName!="") {
        $html = str_replace("</form>", '<input name="stic-recaptcha-id" id="stic-recaptcha-id" type="hidden" value="'.$recaptchaName.'" /></form>', $html);
    }
}
// END STIC-Custom

// STIC-custom JCH 20220312 - Append JS content after download step, to avoid AntiXSS filter
// STIC#633/files#
$jsContent = <<<JS_CONTENT
<script type="text/javascript">

    // STIC-custom 20211122 - jch - Avoid multiple submission
    // STIC#489
    var formHasAlreadyBeenSent = false;
    /**
     * Prevent multiple form submissions
     *
     * @return void
     */
    function lockMultipleSubmissions() {
        if (formHasAlreadyBeenSent) {
            console.log("Form is locked because it has already been sent.");
            event.preventDefault();
        }
        formHasAlreadyBeenSent = true;
    }
JS_CONTENT;
$html .= $jsContent;

// STIC-Custom 20230823 JBL - Add reCAPTCHA validation to forms
// STIC#1180
if ($includeRecaptcha==1 && $recaptchaVersion==2) {
    $jsContent = <<<JS_CONTENT
    // Attach function to event
    document.getElementById("WebToLeadForm").addEventListener("submit", checkCaptcha);

    function checkCaptcha(){
        if(!grecaptcha || grecaptcha.getResponse().length === 0) {
            event.preventDefault();
            formHasAlreadyBeenSent = false;
            if (document.getElementById("g-recaptcha") != null) {
                document.getElementById("g-recaptcha").style.background="red";
            }
        }
    }
    // END STIC-custom
JS_CONTENT;
    $html .= $jsContent;
} else if ($includeRecaptcha==1 && $recaptchaVersion==3) {
    $jsContent = <<<JS_CONTENT
    // Attach function to event
    document.getElementById("WebToLeadForm").addEventListener("submit", prepareCaptcha);

    function prepareCaptcha() {
        event.preventDefault();
        grecaptcha.execute("{$recaptchaWebKey}", {}).then(function (token) {
            // Agregamos el token al campo del formulario
            const recaptchaInput = document.createElement("input");
            recaptchaInput.type = "hidden";
            recaptchaInput.name = "g-recaptcha-response";
            recaptchaInput.value = token;
            recaptchaForm = document.querySelector("#WebToLeadForm");
            recaptchaForm.appendChild(recaptchaInput);
            recaptchaForm.submit();
        });
    }
    // END STIC-custom
JS_CONTENT;
    $html .= $jsContent;
} else {
    $jsContent = <<<JS_CONTENT
    // Attach function to event
    document.getElementById("WebToLeadForm").addEventListener("submit", lockMultipleSubmissions);
    // END STIC-custom
JS_CONTENT;
    $html .= $jsContent;
}
// END STIC-Custom 20230823 JBL

$jsContent = <<<JS_CONTENT

    function submit_form() {
        if (typeof(validateCaptchaAndSubmit) != "undefined") {
            validateCaptchaAndSubmit();
        } else {
            check_webtolead_fields();
            //document.WebToLeadForm.submit();
        }
    }

    function check_webtolead_fields() {
        if (document.getElementById("bool_id") != null) {
            var reqs = document.getElementById("bool_id").value;
            bools = reqs.substring(0, reqs.lastIndexOf(";"));
            var bool_fields = new Array();
            var bool_fields = bools.split(";");
            nbr_fields = bool_fields.length;
            for (var i = 0; i < nbr_fields; i++) {
                if (document.getElementById(bool_fields[i]).value == "on") {
                    document.getElementById(bool_fields[i]).value = 1;
                } else {
                    document.getElementById(bool_fields[i]).value = 0;
                }
            }
        }
    }
</script>
JS_CONTENT;
$html .= $jsContent;
// END STIC

//Check to ensure we have <html> tags in the form. Without them, IE8 will attempt to display the page as XML.
if (stripos((string) $html, "<html") === false) {
    $langHeader = get_language_header();
    $html = "<html {$langHeader}><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></head><body>" . $html . "</body></html>";
}

if (!mb_detect_encoding($html, 'UTF-8')) {
    $html = mb_convert_encoding($html, 'UTF-8', 'ISO-8859-1');
}
$html = str_replace('Â', ' ', $html);
file_put_contents($form_file, $html);
$html = htmlspecialchars($html, ENT_QUOTES | ENT_SUBSTITUTE, 'UTF-8');

$xtpl = new XTemplate('modules/Campaigns/WebToLeadDownloadForm.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);
$webformlink = "<b>$mod_strings[LBL_DOWNLOAD_TEXT_WEB_TO_LEAD_FORM]</b><br/>";
$webformlink .= "<a href=\"index.php?entryPoint=download&id={$guid}&isTempFile=1&tempName=WebToLeadForm.html&type=temp\">$mod_strings[LBL_DOWNLOAD_WEB_TO_LEAD_FORM]</a>";
$xtpl->assign("LINK_TO_WEB_FORM", $webformlink);
$xtpl->assign("RAW_SOURCE", $html);
$xtpl->parse("main.copy_source");
$xtpl->parse("main");
$xtpl->out("main");
