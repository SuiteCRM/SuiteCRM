<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class UserPreferenceTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testgetUserDateTimePreferences(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');

        $result = (new UserPreference($user))->getUserDateTimePreferences();
        self::assertIsArray($result);
    }

    public function testSetAndGetPreference(): void
    {
        self::markTestIncomplete('state is incorrect');

        global $sugar_config;

        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');

        $userPreference = new UserPreference($user);

        //test setPreference method
        $userPreference->setPreference('test', 'test val', 'test_category');

        if (!isset($_SESSION[$user->user_name.'_PREFERENCES']['test_category']['test'])) {
            LoggerManager::getLogger()->warn('no session');
            $result = null;
            self::markTestIncomplete('environment dependency: This test needs session');
        } else {
            $result = $_SESSION[$user->user_name.'_PREFERENCES']['test_category']['test'];
        }

        self::assertEquals('test val', $result);

        //test getPreference method
        $result = $userPreference->getPreference('test', 'test_category');
        self::assertEquals('test val', $result);

        $result = $userPreference->getPreference('chartEngine');
        self::assertEquals($sugar_config['chartEngine'], $result);
    }

    public function testgetDefaultPreference(): void
    {
        global $sugar_config;
        $userPreference = BeanFactory::newBean('UserPreferences');

        //test with non global category
        $result = $userPreference->getDefaultPreference('chartEngine', 'Home');
        self::assertEquals(null, $result);

        //test with default global category

        $result = $userPreference->getDefaultPreference('chartEngine');
        self::assertEquals($sugar_config['chartEngine'], $result);

        $date_format = $sugar_config['datef'] != '' ? $sugar_config['datef'] : $sugar_config['default_date_format'];
        $result = $userPreference->getDefaultPreference('datef');
        self::assertEquals($date_format, $result);

        $time_format = $sugar_config['timef'] != '' ? $sugar_config['timef'] : $sugar_config['default_time_format'];
        $result = $userPreference->getDefaultPreference('timef');
        self::assertEquals($time_format, $result);

        $email_link_type = (isset($sugar_config['email_link_type']) ? $sugar_config['email_link_type'] : null) != '' ? (isset($sugar_config['email_link_type']) ? $sugar_config['email_link_type'] : null) : $sugar_config['email_default_client'];
        $result = $userPreference->getDefaultPreference('email_link_type');
        self::assertEquals($email_link_type, $result);
    }

    public function test__construct(): void
    {
        // execute the constructor and check for the Object type and  attributes
        $userPreference = BeanFactory::newBean('UserPreferences');

        self::assertInstanceOf('UserPreference', $userPreference);
        self::assertInstanceOf('SugarBean', $userPreference);

        self::assertEquals('user_preferences', $userPreference->table_name);
        self::assertEquals('UserPreferences', $userPreference->module_dir);
        self::assertEquals('UserPreference', $userPreference->object_name);

        self::assertEquals(true, $userPreference->new_schema);
        self::assertEquals(true, $userPreference->disable_row_level_security);
    }

    public function testSavePreferencesToDBAndResetPreferences(): void
    {
        self::markTestIncomplete('environment dependency');

        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');

        $userPreference = new UserPreference($user);

        //create a Preference record, save it to DB
        $userPreference->setPreference('test', 'test val', 'test_category');
        $userPreference->savePreferencesToDB();

        //retrieve it back and verify
        $result = $userPreference->retrieve_by_string_fields(array(
                'assigned_user_id' => $user->id,
                'category' => 'test_category',
        ));

        //$this->assertFalse(isset($result->id));

        //reset the preferences and verify that it is deleted
        $userPreference->resetPreferences();
        $result = $userPreference->retrieve_by_string_fields(array(
                'assigned_user_id' => $user->id,
                'category' => 'test_category',
        ));
        self::assertEquals(null, $result);
    }

    public function testupdateAllUserPrefs(): void
    {
        global $current_user;

        $current_user = BeanFactory::newBean('Users');
        $current_user->retrieve('1');

        //UserPreference::updateAllUserPrefs("test","test val");

        self::markTestIncomplete('Multiple errors in method: Unknown column user_preferences in field list');
    }
}
