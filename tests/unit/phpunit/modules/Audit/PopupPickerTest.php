<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'modules/Audit/Popup_picker.php';

class PopupPickerTest extends SuitePHPUnitFrameworkTestCase
{
    public function testProcessPage(): void
    {
        global $focus;
        $focus = BeanFactory::getBean('Contacts');
        $focus->name = 'test';
        $focus->save();

        ob_start();
        $result = (new Popup_Picker())->process_page();
        $output = ob_get_contents();
        ob_end_clean();
        self::assertNull($result);
        self::assertNotEmpty($output);
        self::assertStringContainsStringIgnoringCase('<!DOCTYPE HTML>', $output);
        self::assertStringContainsStringIgnoringCase('<html lang=\'en_us\'>', $output);
        self::assertStringContainsStringIgnoringCase('<title>SuiteCRM - Open Source CRM</title>', $output);
        self::assertStringContainsStringIgnoringCase('<link rel="stylesheet" type="text/css" href="cache/themes/SuiteP/css/Dawn/style.css', $output);
        self::assertStringContainsStringIgnoringCase('<meta http-equiv="Content-Type" content="text/html; charset="{$charset}">', $output);
        self::assertStringContainsStringIgnoringCase('<body class="popupBody">', $output);
        self::assertStringContainsStringIgnoringCase('<div class=\'moduleTitle\'>', $output);
        self::assertStringContainsStringIgnoringCase('<img src="themes/default/images/print.gif', $output);
        self::assertStringContainsStringIgnoringCase('Fields audited in this module: Lawful Basis Date Reviewed, Do Not Call, Office Phone, Assigned User, Lawful Basis, Lawful Basis Source', $output);
    }
}
