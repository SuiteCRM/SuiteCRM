<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

    $db = DBManagerFactory::getInstance();

    $even_id = $db->quote($_GET['event'] ?? '');
    $delegate_id =$db->quote( $_GET['delegate'] ?? '');
    $type = $db->quote($_GET['type'] ?? '');
    $response = $db->quote($_GET['response'] ?? '');

    //get event
    $event = BeanFactory::newBean('FP_events');
    $event->retrieve($even_id);
    
    if ($type == 'c') {
        $event->load_relationship('fp_events_contacts'); // get related contacts

        if ($response == 'accept') {

            //check to see if they have already responded to the email
            $check_q = 'SELECT email_responded FROM fp_events_contacts_c WHERE fp_events_contactsfp_events_ida="'.$event->id.'" AND fp_events_contactscontacts_idb="'.$delegate_id.'"';
            $check = $db->getOne($check_q);
            //update contact to accepted
            $query = 'UPDATE fp_events_contacts_c SET accept_status="Accepted", email_responded="1" WHERE fp_events_contactsfp_events_ida="'.$event->id.'" AND fp_events_contactscontacts_idb="'.$delegate_id.'" AND email_responded="0"';
            if ($db->query($query) && $check != '1') {
                if (!IsNullOrEmptyString($event->accept_redirect)) {
                    $url = $event->accept_redirect;
                    header('Location: ' . $url);
                } else {
                    echo 'Thank you for accepting';
                }
            } else {
                echo 'You have already responded to the invitation or there was a problem with the link. Please contact the sender of the invite for help.';
            }
        } else {
            if ($response == 'decline') {
                //check to see if they have already responded to the email
                $check_q = 'SELECT email_responded FROM fp_events_contacts_c WHERE fp_events_contactsfp_events_ida="'.$event->id.'" AND fp_events_contactscontacts_idb="'.$delegate_id.'"';
                $check = $db->getOne($check_q);
                //update contact to accepted
                $query = 'UPDATE fp_events_contacts_c SET accept_status="Declined", email_responded="1" WHERE fp_events_contactsfp_events_ida="'.$event->id.'" AND fp_events_contactscontacts_idb="'.$delegate_id.'" AND email_responded="0"';

                if ($db->query($query) && $check != '1') {
                    if (!IsNullOrEmptyString($event->decline_redirect)) {
                        $url = $event->decline_redirect;
                        header('Location: ' . $url);
                    } else {
                        echo 'Thank you for declining';
                    }
                } else {
                    echo 'You have already responded to the invitation or there was a problem with the link. Please contact the sender of the invite for help.';
                }
            }
        }
    }
    if ($type == 't') {
        $event->load_relationship('fp_events_prospects_1'); //get related targets

        if ($response == 'accept') {
            //check to see if they have already responded to the email
            $check_q = 'SELECT email_responded FROM fp_events_prospects_1_c WHERE fp_events_prospects_1fp_events_ida="'.$event->id.'" AND fp_events_prospects_1prospects_idb="'.$delegate_id.'"';
            $check = $db->getOne($check_q);

            //update contact to accepted
            $query = 'UPDATE fp_events_prospects_1_c SET accept_status="Accepted", email_responded="1" WHERE fp_events_prospects_1fp_events_ida="'.$event->id.'" AND fp_events_prospects_1prospects_idb="'.$delegate_id.'" AND email_responded="0"';
            if ($db->query($query) && $check != '1') {
                if (!IsNullOrEmptyString($event->accept_redirect)) {
                    $url = $event->accept_redirect;
                    header('Location: ' . $url);
                } else {
                    echo 'Thank you for accepting';
                }
            } else {
                echo 'You have already responded to the invitation or there was a problem with the link. Please contact the sender of the invite for help.';
            }
        } else {
            if ($response == 'decline') {
                //check to see if they have already responded to the email
                $check_q = 'SELECT email_responded FROM fp_events_prospects_1_c WHERE fp_events_prospects_1fp_events_ida="'.$event->id.'" AND fp_events_prospects_1prospects_idb="'.$delegate_id.'"';
                $check = $db->getOne($check_q);
                //update contact to accepted
                $query = 'UPDATE fp_events_prospects_1_c SET accept_status="Declined", email_responded="1" WHERE fp_events_prospects_1fp_events_ida="'.$event->id.'" AND fp_events_prospects_1prospects_idb="'.$delegate_id.'" AND email_responded="0"';
                if ($db->query($query) && $check != '1') {
                    if (!IsNullOrEmptyString($event->decline_redirect)) {
                        $url = $event->decline_redirect;
                        header('Location: ' . $url);
                    } else {
                        echo 'Thank you for declining';
                    }
                } else {
                    echo 'You have already responded to the invitation or there was a problem with the link. Please contact the sender of the invite for help.';
                }
            }
        }
    }
    if ($type == 'l') {
        $event->load_relationship('fp_events_leads_1'); //get related leads

        if ($response == 'accept') {
            //check to see if they have already responded to the email
            $check_q = 'SELECT email_responded FROM fp_events_leads_1_c WHERE ffp_events_leads_1fp_events_ida="'.$event->id.'" AND fp_events_leads_1leads_idb="'.$delegate_id.'"';
            $check = $db->getOne($check_q);
            //update contact to accepted
            $query = 'UPDATE fp_events_leads_1_c SET accept_status="Accepted", email_responded="1" WHERE fp_events_leads_1fp_events_ida="'.$event->id.'" AND fp_events_leads_1leads_idb="'.$delegate_id.'" AND email_responded="0"';
            if ($db->query($query) && $check != '1') {
                if (!IsNullOrEmptyString($event->accept_redirect)) {
                    $url = $event->accept_redirect;
                    header('Location: ' . $url);
                } else {
                    echo 'Thank you for accepting';
                }
            } else {
                echo 'There was a problem with the link please contact the sender of the invite';
            }
        } else {
            if ($response == 'decline') {
                //check to see if they have already responded to the email
                $check_q = 'SELECT email_responded FROM fp_events_leads_1_c WHERE fp_events_leads_1fp_events_ida="'.$event->id.'" AND fp_events_leads_1leads_idb="'.$delegate_id.'"';
                $check = $db->getOne($check_q);
                //update contact to accepted
                $query = 'UPDATE fp_events_leads_1_c SET accept_status="Declined", email_responded="1" WHERE fp_events_leads_1fp_events_ida="'.$event->id.'" AND fp_events_leads_1leads_idb="'.$delegate_id.'" AND email_responded="0"';

                if ($db->query($query) && $check != '1') {
                    if (!IsNullOrEmptyString($event->decline_redirect)) {
                        $url = $event->decline_redirect;
                        header('Location: ' . $url);
                    } else {
                        echo 'Thank you for declining';
                    }
                } else {
                    echo 'There was a problem with the link please contact the sender of the invite';
                }
            }
        }
    }
    // Function for basic field validation (present and neither empty nor only white space nor just 'http://')
    function IsNullOrEmptyString($question)
    {
        return (!isset($question) || trim($question)==='' || $question =='http://');
    }
