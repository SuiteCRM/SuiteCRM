<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

function handleAttachmentForRemove()
{
    if (!empty($_REQUEST['attachmentsRemove'])) {
        foreach ($_REQUEST['attachmentsRemove'] as $attachmentIdForRemove) {
            if ($bean = BeanFactory::getBean('Notes', $attachmentIdForRemove)) {
                $bean->mark_deleted($bean->id);
            }
        }
    }
}

$error = false;
$msgs = array();
$data = array();

$emailTemplateId = isset($_REQUEST['emailTemplateId']) && $_REQUEST['emailTemplateId'] ? $_REQUEST['emailTemplateId'] : null;
if (isset($_REQUEST['campaignId'])) {
    $_SESSION['campaignWizard'][$_REQUEST['campaignId']]['defaultSelectedTemplateId'] = $emailTemplateId;
}

if (preg_match('/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/', (string) $emailTemplateId) || !$emailTemplateId) {
    $func = isset($_REQUEST['func']) ? $_REQUEST['func'] : null;

    $fields = array('body_html', 'subject', 'name');

    // TODO: validate for email template before save it!

    include_once 'modules/EmailTemplates/EmailTemplateFormBase.php';

    switch ($func) {

        case 'update':
            $bean = BeanFactory::getBean('EmailTemplates', $emailTemplateId);
            foreach ($bean as $key => $value) {
                if (in_array($key, $fields)) {
                    $bean->$key = $_POST[$key];
                }
            }
            $formBase = new EmailTemplateFormBase();
            $bean = $formBase->handleAttachmentsProcessImages($bean, false, true, 'download', true);
            if ($bean->save()) {
                $msgs[] = 'LBL_TEMPLATE_SAVED';
            }
            //$formBase = new EmailTemplateFormBase();
            //$bean = $formBase->handleAttachmentsProcessImages($bean, false, true);
            $data['id'] = $bean->id;
            $data['name'] = $bean->name;
            handleAttachmentForRemove();

            // update marketing->template_id if we have a selected marketing..
            if (!empty($_REQUEST['campaignId']) && !empty($_SESSION['campaignWizard'][$_REQUEST['campaignId']]['defaultSelectedMarketingId'])) {
                $marketingId = $_SESSION['campaignWizard'][$_REQUEST['campaignId']]['defaultSelectedMarketingId'];

                $campaign = BeanFactory::getBean('Campaigns', $_REQUEST['campaignId']);
                $campaign->load_relationship('emailmarketing');
                $marketings = $campaign->emailmarketing->get();
                // just a double check for campaign->marketing relation correct is for e.g the user deleted the marketing record or something may could happened..
                if (in_array($marketingId, $marketings)) {
                    $marketing = BeanFactory::getBean('EmailMarketing', $marketingId);
                    $marketing->template_id = $emailTemplateId;
                    $marketing->save();
                } else {
                    // TODO something is not OK, the selected campaign isn't related to this marketing!!
                    $GLOBALS['log']->debug('Selected marketing not found!');
                }
            }
            break;

        case 'createCopy':
            $bean = BeanFactory::getBean('EmailTemplates', $emailTemplateId);
            $newBean = BeanFactory::newBean('EmailTemplates');
            $fieldsForCopy = array('type', 'description');
            foreach ($bean as $key => $value) {
                if (in_array($key, $fields)) {
                    $newBean->$key = $_POST[$key];
                } else {
                    if (in_array($key, $fieldsForCopy)) {
                        $newBean->$key = $bean->$key;
                    }
                }
            }
            $newBean->assigned_user_id = $GLOBALS['current_user']->id;
            if ($newBean->save()) {
                $msgs[] = 'LBL_TEMPLATE_SAVED';
            }
            //$formBase = new EmailTemplateFormBase();
            //$newBean = $formBase->handleAttachmentsProcessImages($newBean, false, true);
            $data['id'] = $newBean->id;
            $data['name'] = $newBean->name;
            break;

        case 'uploadAttachments':
            $formBase = new EmailTemplateFormBase();
            $focus = BeanFactory::getBean('EmailTemplates', $_REQUEST['attach_to_template_id']);
            //$data = $formBase->handleAttachments($focus, false, null);
            $data = $formBase->handleAttachmentsProcessImages($focus, false, true, 'download', true);
            $redirectUrl = 'index.php?module=Campaigns&action=WizardMarketing&campaign_id=' . $_REQUEST['campaign_id'] . "&jump=2&template_id=" . $_REQUEST['attach_to_template_id']; // . '&marketing_id=' . $_REQUEST['attach_to_marketing_id'] . '&record=' . $_REQUEST['attach_to_marketing_id'];
            header('Location: ' . $redirectUrl);
            die();
            break;

        default: case 'get':
            if ($bean = BeanFactory::getBean('EmailTemplates', $emailTemplateId)) {
                $fields = array('id', 'name', 'body', 'body_html', 'subject');
                foreach ($bean as $key => $value) {
                    if (in_array($key, $fields)) {
                        $data[$key] = $bean->$key;
                    }
                }

                $data['body_from_html'] = from_html($bean->body_html);
                $attachmentBeans = $bean->getAttachments();
                if ($attachmentBeans) {
                    $attachments = array();
                    foreach ($attachmentBeans as $attachmentBean) {
                        $attachments[] = array(
                            'id' => $attachmentBean->id,
                            'name' => $attachmentBean->name,
                            'file_mime_type' => $attachmentBean->file_mime_type,
                            'filename' => $attachmentBean->filename,
                            'parent_type' => $attachmentBean->parent_type,
                            'parent_id' => $attachmentBean->parent_id,
                            'description' => $attachmentBean->description,
                        );
                    }
                    $data['attachments'] = $attachments;
                }
            } else {
                $error = 'Email Template not found.';
            }
            break;
    }
} else {
    $error = 'Illegal GUID format.';
}

$results = array(
    'error' => $error,
    'msgs' => $msgs,
    'data' => $data,
);

$results = json_encode($results);
if (!$results) {
    if (json_last_error()) {
        $results = array(
            'error' => 'json_encode error: '.json_last_error_msg()
        );
        $results = json_encode($results);
    }
}
echo $results;
