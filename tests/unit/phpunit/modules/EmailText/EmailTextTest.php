<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class EmailTextTest extends SuitePHPUnitFrameworkTestCase
{
    public function testEmailText(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $emailText = BeanFactory::newBean('EmailText');

        self::assertInstanceOf('EmailText', $emailText);
        self::assertInstanceOf('SugarBean', $emailText);

        self::assertEquals('EmailText', $emailText->module_dir);
        self::assertEquals('EmailText', $emailText->module_name);
        self::assertEquals('EmailText', $emailText->object_name);
        self::assertEquals('emails_text', $emailText->table_name);
        self::assertEquals(true, $emailText->disable_row_level_security);
        self::assertEquals(true, $emailText->disable_custom_fields);
    }
}
