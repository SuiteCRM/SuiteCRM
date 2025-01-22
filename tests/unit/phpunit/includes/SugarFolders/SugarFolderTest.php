<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SugarFolderTest extends SuitePHPUnitFrameworkTestCase
{
    protected $folderId = null;

    protected function setUp(): void
    {
        parent::setUp();
        include_once __DIR__ . '/../../../../../modules/Users/User.php';
        include_once __DIR__ . '/../../../../../include/SugarFolders/SugarFolders.php';
    }

    /**
     * Test the object can be found
     *
     * @return void
     */
    public function testCanGetSugarFolderObject(): void
    {
        $sugarfolder = new SugarFolder();

        self::assertIsObject($sugarfolder);
    }

    public function testGenerateArchiveFolderQuery(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sql = (new SugarFolder($user))->generateArchiveFolderQuery();

        $expected = "SELECT emails.id , emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, 'Emails' polymorphic_module FROM emails JOIN emails_text on emails.id = emails_text.email_id JOIN emails_email_addr_rel eear ON eear.email_id = emails.id "
            . "JOIN email_addr_bean_rel eabr ON eabr.email_address_id=eear.email_address_id WHERE emails.deleted=0 AND emails.type NOT IN ('out', 'draft') AND emails.status NOT IN ('sent', 'draft') AND eabr.bean_id = '1' AND eabr.bean_module = 'Users' AND eear.deleted=0 GROUP BY id";

        self::assertEquals($expected, $sql);
    }

    public function testGenerateSugarsDynamicFolderQuery(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarfolder = new SugarFolder($user);
        $sugarfolder->folder_type = 'archived';

        $sql = $sugarfolder->generateSugarsDynamicFolderQuery();

        $expected = "SELECT emails.id , emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, 'Emails' polymorphic_module FROM emails JOIN emails_text on emails.id = emails_text.email_id JOIN emails_email_addr_rel eear ON eear.email_id = emails.id "
            . "JOIN email_addr_bean_rel eabr ON eabr.email_address_id=eear.email_address_id WHERE emails.deleted=0 AND emails.type NOT IN ('out', 'draft') AND emails.status NOT IN ('sent', 'draft') AND eabr.bean_id = '1' AND eabr.bean_module = 'Users' AND eear.deleted=0 GROUP BY id";

        self::assertEquals($sql, $expected);

        $sugarfolder->folder_type = 'sent';

        $sql = $sugarfolder->generateSugarsDynamicFolderQuery();

        $expected = "SELECT emails.id, emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, 'Emails' polymorphic_module FROM emails JOIN emails_text on emails.id = emails_text.email_id WHERE (type = 'out' OR status = 'sent') AND assigned_user_id = '1' AND emails.deleted = 0 AND emails.status NOT IN ('archived') AND emails.type NOT IN ('archived')";

        self::assertEquals($sql, $expected);

        $sugarfolder->folder_type = 'inbound';

        $sql = $sugarfolder->generateSugarsDynamicFolderQuery();

        $expected = "SELECT emails.id, emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status, emails_text.from_addr, emails_text.to_addrs, 'Emails' polymorphic_module FROM emails JOIN emails_text on emails.id = emails_text.email_id WHERE (type = 'inbound' OR status = 'inbound') AND assigned_user_id = '1' AND emails.deleted = 0 AND emails.status NOT IN ('sent', 'archived', 'draft') AND emails.type NOT IN ('out', 'archived', 'draft')";

        self::assertEquals($sql, $expected);
    }

    public function testFolderSubscriptions(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarfolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Test folder',
            'parent_folder'    => ''
        );

        $saved = $sugarfolder->setFolder($fields);

        self::assertTrue($saved);

        $sugarfolder->insertFolderSubscription($sugarfolder->id, $user->id);

        $subscriptions = $sugarfolder->getSubscriptions($user);

        self::assertTrue(((is_countable($subscriptions) ? count($subscriptions) : 0) > 0));

        // Access clear with user
        $sugarfolder->clearSubscriptions($user);

        // Access with default param
        $sugarfolder->clearSubscriptions();

        $subscriptions = $sugarfolder->getSubscriptions(null);

        self::assertCount(0, $subscriptions);

        // test the add subscription to group folder
        $sugarfolder->addSubscriptionsToGroupFolder();

        // $sugarfolder->createSubscriptionForUser($user_id);
        // $sugarfolder->clearSubscriptionsForFolder($folder_id);
        // $sugarfolder->getSubscriptions($user);
        // $sugarfolder->insertFolderSubscription($folderId, $userID)
        // $sugarfolder->clearSubscriptions($user = null)
    }

    public function testClearSubscriptionsForFolder(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarfolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Test folder',
            'parent_folder'    => ''
        );

        $saved = $sugarfolder->setFolder($fields);

        self::assertTrue($saved);

        $sugarfolder->insertFolderSubscription($sugarfolder->id, $user->id);

        $sugarfolder->clearSubscriptionsForFolder($sugarfolder->id);
    }

    public function testGetFoldersForSettings(): void
    {
        self::markTestIncomplete('#Error: mysqli_real_escape_string(): Couldn\'t fetch mysqli');
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarfolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Test folder',
            'parent_folder'    => ''
        );

        $saved = $sugarfolder->setFolder($fields);

        self::assertTrue($saved);

        $saved = false;

        $childSugarFolder = new SugarFolder($user);

        $fields = array(
            'name'          => 'Child Test folder',
            'parent_folder' => $sugarfolder->id
        );

        $saved = $childSugarFolder->setFolder($fields);

        self::assertTrue($saved);

        $sugarfolder->has_child = 1;

        $ret = $sugarfolder->getFoldersForSettings();

        self::assertIsArray($ret);

        $ret = $childSugarFolder->getParentIDRecursive($childSugarFolder->id);

        self::assertContains($sugarfolder->id, $ret);
    }

    public function testCrudFolder(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarfolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Test folder',
            'parent_folder'    => ''
        );

        $saved = $sugarfolder->setFolder($fields);

        self::assertTrue($saved);

        $saved = false;

        $childSugarFolder = new SugarFolder($user);

        $fields = array(
            'name'          => 'Child Test folder',
            'parent_folder' => $sugarfolder->id
        );

        $saved = $childSugarFolder->setFolder($fields);

        self::assertTrue($saved);

        $sugarfolder->has_child = 1;

        $update = array(
            'record' => $sugarfolder->id,
            'name'   => 'Test Folder',
            'parent_folder' => ''
        );

        $ret = $sugarfolder->updateFolder($update);

        self::assertEquals('done', $ret['status']);

        // Try to update the child with parent

        $update = array(
            'record' => $sugarfolder->id,
            'name'   => 'Test Folder',
            'parent_folder' => $childSugarFolder->id
        );

        $ret = $sugarfolder->updateFolder($update);

        self::assertEquals('failed', $ret['status']);

        $deleted = $sugarfolder->delete();

        self::assertTrue($deleted);
    }

    public function testCheckFalseIdForDelete(): void
    {
        $ret = (new SugarFolder())->delete();

        self::assertFalse($ret);
    }

    public function testCopyBean(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder One',
            'parent_folder'    => ''
        );

        $saved = $parentFolderOne->setFolder($fields);

        self::assertTrue($saved);

        $parentFolderTwo = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder Two',
            'parent_folder'    => ''
        );

        $saved = $parentFolderTwo->setFolder($fields);

        self::assertTrue($saved);

        $bean = BeanFactory::newBean('Emails');
        $bean->save();

        $parentFolderOne->addBean($bean);

        $parentFolderOne->copyBean($parentFolderOne->id, $parentFolderTwo->id, $bean->id, $bean->module_name);

        $existInFolderOne = $parentFolderOne->checkEmailExistForFolder($bean->id);
        $existInFolderTwo = $parentFolderTwo->checkEmailExistForFolder($bean->id);

        self::assertTrue($existInFolderOne);

        self::assertTrue($existInFolderTwo);
    }


    public function testMoveFolder(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder One',
            'parent_folder'    => ''
        );

        $saved = $parentFolderOne->setFolder($fields);

        self::assertTrue($saved);

        $parentFolderTwo = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder Two',
            'parent_folder'    => ''
        );

        $saved = $parentFolderTwo->setFolder($fields);

        self::assertTrue($saved);

        $childFolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Child Folder',
            'parent_folder'    => $parentFolderOne->id
        );

        $saved = $childFolder->setFolder($fields);

        self::assertTrue($saved);

        $success = $parentFolderOne->move($parentFolderOne->id, $parentFolderTwo->id, $childFolder->id);

        self::assertTrue($success);
    }

    public function testGetListItemsForEmailXML(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $sugarFolder = new SugarFolder($user);
        $sugarFolder->name = 'Parent Folder';
        $saved = $sugarFolder->save();

        self::assertTrue($saved);
        $saved = false;

        $arrayOfEmailBeans = [];
        for ($i = 0; $i < 10; $i++) {
            $arrayOfEmailBeans[$i] = BeanFactory::newBean('Emails');
            $arrayOfEmailBeans[$i]->save();

            $sugarFolder->addBean($arrayOfEmailBeans[$i]);
        }

        $results = $sugarFolder->getListItemsForEmailXML($sugarFolder->id);
        self::assertIsArray($results);
        $results = false;

        $dynamicSugarFolder = new SugarFolder($user);
        $dynamicSugarFolder->name = 'Dynamic Parent Folder';

        $dynamicSugarFolder->is_dynamic = true;
        $dynamicSugarFolder->is_group   = true;
        $dynamicSugarFolder->hrSortLocal = array();

        $saved = $dynamicSugarFolder->save();
        self::assertTrue($saved);

        for ($i = 0; $i < 10; $i++) {
            $arrayOfEmailBeans[$i] = BeanFactory::newBean('Emails');
            $arrayOfEmailBeans[$i]->save();

            $dynamicSugarFolder->addBean($arrayOfEmailBeans[$i]);
        }

        $results = $dynamicSugarFolder->getListItemsForEmailXML($dynamicSugarFolder->id);
        self::assertIsArray($results);
    }

    public function testCountOfItems(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder',
            'parent_folder'    => ''
        );

        $saved = $parentFolder->setFolder($fields);
        self::assertTrue($saved);

        $saved = false;

        $arrayOfEmailBeans = array();

        $deadBean = BeanFactory::newBean('Emails');
        $parentFolder->addBean($deadBean);

        for ($i = 0; $i < 10; $i++) {
            $arrayOfEmailBeans[$i] = BeanFactory::newBean('Emails');
            $arrayOfEmailBeans[$i]->save();

            $parentFolder->addBean($arrayOfEmailBeans[$i]);
        }

        $count = $parentFolder->getCountItems($parentFolder->id);

        self::assertEquals(10, $count);

        // Test getCountNewItems
        $count = $parentFolder->getCountNewItems($parentFolder->id, false, false);
        self::assertEquals(0, $count);

        $count = $parentFolder->getCountUnread($parentFolder->id);
        self::assertEquals(0, $count);


        // non-existent folder
        $newBean = BeanFactory::newBean('Emails');
        $newBean->save();

        $parentFolder->id = null;
        $parentFolder->addBean($newBean);

        // create with dynamic and group

        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolder = new SugarFolder($user);

        $parentFolder->name       = 'Parent Folder';
        $parentFolder->is_dynamic = true;
        $parentFolder->is_group   = true;

        $saved = $parentFolder->save();

        self::assertTrue($saved);
        $saved = false;

        $arrayOfEmailBeans = array();

        for ($i = 0; $i < 10; $i++) {
            $arrayOfEmailBeans[$i] = BeanFactory::newBean('Emails');
            $arrayOfEmailBeans[$i]->save();

            $parentFolder->addBean($arrayOfEmailBeans[$i]);
        }

        $count = $parentFolder->getCountItems($parentFolder->id);
        self::assertEquals(0, $count);

        $count = $parentFolder->getCountUnread($parentFolder->id);
        self::assertEquals(0, $count);
    }

    public function testNonExistingRetrieve(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolder = new SugarFolder($user);

        $randomGuid = 'randstring';

        $ret = $parentFolder->retrieve($randomGuid);

        self::assertFalse($ret);
    }

    public function testDeleteEmailsFromFolder(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolder = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder',
            'parent_folder'    => ''
        );

        $saved = $parentFolder->setFolder($fields);

        self::assertTrue($saved);

        $bean = BeanFactory::newBean('Emails');
        $bean->save();

        $parentFolder->addBean($bean);

        $count = $parentFolder->getCountItems($parentFolder->id);

        self::assertEquals(1, $count);

        $parentFolder->deleteEmailFromFolder($bean->id);
    }


    public function testDeleteEmailsFromAllFolders(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder One',
            'parent_folder'    => ''
        );

        $saved = $parentFolderOne->setFolder($fields);

        self::assertTrue($saved);

        // reset saved
        $saved = false;

        $parentFolderTwo = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder Two',
            'parent_folder'    => ''
        );

        $saved = $parentFolderTwo->setFolder($fields);

        self::assertTrue($saved);

        $bean = BeanFactory::newBean('Emails');
        $bean->save();

        $parentFolderOne->addBean($bean);

        $parentFolderOne->copyBean($parentFolderOne->id, $parentFolderTwo->id, $bean->id, $bean->module_name);

        $existInFolderOne = $parentFolderOne->checkEmailExistForFolder($bean->id);
        $existInFolderTwo = $parentFolderTwo->checkEmailExistForFolder($bean->id);

        self::assertTrue($existInFolderOne);
        self::assertTrue($existInFolderTwo);

        $parentFolderOne->deleteEmailFromAllFolder($bean->id);

        $existInFolderOne = $parentFolderOne->checkEmailExistForFolder($bean->id);
        $existInFolderTwo = $parentFolderTwo->checkEmailExistForFolder($bean->id);

        self::assertFalse($existInFolderOne);
        self::assertFalse($existInFolderTwo);
    }

    public function testGetUserFolders(): void
    {
        self::markTestIncomplete(
            'This test requires a review'
        );

        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $parentFolderOne->name = 'Parent Folder';
        $parentFolderOne->folder_type = 'inbound';

        $saved = $parentFolderOne->save();

        self::assertTrue($saved);

        $arrayOfEmailBeans = array();

        for ($i = 0; $i < 10; $i++) {
            $arrayOfEmailBeans[$i] = BeanFactory::newBean('Emails');
            $arrayOfEmailBeans[$i]->save();

            $parentFolderOne->addBean($arrayOfEmailBeans[$i]);
        }

        $childFolder = new SugarFolder($user);

        $fields = array(
            'name' => 'Child Folder',
            'parent_folder' => $parentFolderOne->id,
        );

        $subChildFolderOne = new SugarFolder($user);

        $fields = array(
            'name' => 'Sub Child Folder One',
            'parent_folder' => $childFolder->id,
        );

        $subChildFolderTwo = new SugarFolder($user);

        $fields = array(
            'name' => 'Sub Child Folder Two',
            'parent_folder' => $childFolder->id,
        );

        $anotherChildFolder = new SugarFolder($user);

        $fields = array(
            'name' => 'Another Child Folder',
            'parent_folder' => $parentFolderOne->id,
        );

        $subs = array($anotherChildFolder->id, $parentFolderOne->id, $childFolder->id, $subChildFolderOne->id, $subChildFolderTwo->id);
        $parentFolderOne->setSubscriptions($subs);

        $email = BeanFactory::newBean('Emails');
        $email->email2init();

        $ie = BeanFactory::newBean('InboundEmail');
        $ie->email = $email;

        $rootNode = new ExtNode('', '');

        $folderOpenState = $user->getPreference('folderOpenState', 'Emails');
        $folderOpenState = empty($folderOpenState) ? '' : $folderOpenState;

        $parentFolderOne->getUserFolders($rootNode, sugar_unserialize($folderOpenState), null, true);

        self::assertIsObject($rootNode);
    }

    public function testSetSubscriptionWithNoUser(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder',
            'parent_folder'    => ''
        );

        $saved = $parentFolderOne->setFolder($fields);

        self::assertTrue($saved);

        $subs = array($parentFolderOne->id);

        $ret = $parentFolderOne->setSubscriptions($subs, 0);

        self::assertFalse($ret);
    }

    public function testUpdateSave(): void
    {
        $user = BeanFactory::newBean('Users');
        $user->id = 1;

        $parentFolderOne = new SugarFolder($user);

        $fields = array(
            'name'             => 'Parent Folder',
            'parent_folder'    => ''
        );

        $saved = $parentFolderOne->setFolder($fields);

        self::assertTrue($saved);

        $parentFolderOne->new_with_id = true;
        $parentFolderOne->save();
    }
}
