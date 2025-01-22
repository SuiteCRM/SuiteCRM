<?php /** @noinspection ALL */
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

use Mockery as m;
use SuiteCRM\Search\Index\Documentify\SearchDefsDocumentifier;
use SuiteCRM\Tests\Unit\lib\SuiteCRM\Search\SearchTestAbstract;

class SearchDefsDocumentifierTest extends SearchTestAbstract
{
    public function testGetFieldsToIndex()
    {
        $documentifier = new SearchDefsDocumentifier();

        $mockModule = 'MockModule';

        $mockParser = $this->getMockParser($mockModule);
        $expected = $this->getExpectedFields();

        $actual = self::invokeMethod($documentifier, 'getFieldsToIndex', [$mockModule, $mockParser]);

        self::assertEquals($expected, $actual);
    }

    public function testDocumentifyContact()
    {
        $documentifier = new SearchDefsDocumentifier();

        $module = 'Contacts';

        $mockParser = $this->getMockParser($module);

        /** @var \Contact $contact */
        $contact = BeanFactory::newBean('Contacts');

        $contact->first_name = 'Foo';
        $contact->last_name = 'Bar';
        $contact->alt_address_city = 'FooCity';
        $contact->phone_fax = '132';

        $expected = [
            'name' => [
                'first' => 'Foo',
                'last' => 'Bar',
            ],
            'phone' =>
                [
                    'fax' => '132',
                ],
            'address' =>
                [
                    'alt' => [
                        'city' => 'FooCity',
                    ],
                ],
        ];

        $actual = $documentifier->documentify($contact, $mockParser);

        self::assertEquals($expected, $actual);
    }

    public function testDocumentifyAccount()
    {
        $documentifier = new SearchDefsDocumentifier();

        /** @var Account $account */
        $account = BeanFactory::newBean('Accounts');

        $account->name = 'SuperDogs Ldt.';
        $account->phone_office = '123456789';
        $account->annual_revenue = '123 (USD)';
        $account->billing_address_city = 'FooCity';
        $account->billing_address_country = 'FooCountry';
        $account->billing_address_postalcode = 'FooPostalCode';

        $actual = $documentifier->documentify($account);
        $expected = [
            'name' =>
                [
                    'name' => 'SuperDogs Ldt.',
                ],
            'annual_revenue' => '123 (USD)',
            'address' => [
                'billing' => [
                    'city' => 'FooCity',
                    'postalcode' => 'FooPostalCode',
                    'country' => 'FooCountry',
                ],
            ],
            'phone' =>
                [
                    'office' => '123456789',
                ],
        ];

        self::assertEquals($expected, $actual);
    }

    /**
     * @param string $mockModule
     * @return array
     */
    private function getFields($mockModule)
    {
        return [
            $mockModule => [
                'first_name' =>
                    [
                        'query_type' => 'default',
                    ],
                'last_name' =>
                    [
                        'query_type' => 'default',
                    ],
                'search_name' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'first_name',
                                'last_name',
                            ],
                        'force_unifiedsearch' => true,
                    ],
                'account_name' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'accounts.name',
                            ],
                    ],
                'lead_source' =>
                    [
                        'query_type' => 'default',
                        'operator' => '=',
                        'options' => 'lead_source_dom',
                        'template_var' => 'LEAD_SOURCE_OPTIONS',
                    ],
                'do_not_call' =>
                    [
                        'query_type' => 'default',
                        'input_type' => 'checkbox',
                        'operator' => '=',
                    ],
                'phone' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'phone_mobile',
                                'phone_work',
                                'phone_other',
                                'phone_fax',
                                'assistant_phone',
                            ],
                    ],
                'email' =>
                    [
                        'query_type' => 'default',
                        'operator' => 'subquery',
                        'subquery' => 'SELECT eabr.bean_id FROM email_addr_bean_rel eabr JOIN email_addresses ea ON (ea.id = eabr.email_address_id) WHERE eabr.deleted=0 AND ea.email_address LIKE',
                        'db_field' =>
                            [
                                'id',
                            ],
                    ],
                'optinprimary' =>
                    [
                        'type' => 'enum',
                        'options' => 'email_confirmed_opt_in_dom',
                        'query_type' => 'default',
                        'operator' => 'subquery',
                        'subquery' => 'SELECT eabr.bean_id FROM email_addr_bean_rel eabr JOIN email_addresses ea ON (ea.id = eabr.email_address_id) WHERE eabr.deleted=0 AND eabr.primary_address = \'1\' AND ea.confirm_opt_in LIKE',
                        'db_field' =>
                            [
                                'id',
                            ],
                        'vname' => 'LBL_OPT_IN_FLAG_PRIMARY',
                    ],
                'favorites_only' =>
                    [
                        'query_type' => 'format',
                        'operator' => 'subquery',
                        'checked_only' => true,
                        'subquery' => 'SELECT favorites.parent_id FROM favorites
			                    WHERE favorites.deleted = 0
			                        and favorites.parent_type = \'Contacts\'
			                        and favorites.assigned_user_id = \'{1}\'',
                        'db_field' =>
                            [
                                'id',
                            ],
                    ],
                'assistant' =>
                    [
                        'query_type' => 'default',
                    ],
                'address_street' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'primary_address_street',
                                'alt_address_street',
                            ],
                    ],
                'address_city' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'primary_address_city',
                                'alt_address_city',
                            ],
                    ],
                'address_state' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'primary_address_state',
                                'alt_address_state',
                            ],
                    ],
                'address_postalcode' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'primary_address_postalcode',
                                'alt_address_postalcode',
                            ],
                    ],
                'address_country' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'primary_address_country',
                                'alt_address_country',
                            ],
                    ],
                'current_user_only' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'assigned_user_id',
                            ],
                        'my_items' => true,
                        'vname' => 'LBL_CURRENT_USER_FILTER',
                        'type' => 'bool',
                    ],
                'assigned_user_id' =>
                    [
                        'query_type' => 'default',
                    ],
                'account_id' =>
                    [
                        'query_type' => 'default',
                        'db_field' =>
                            [
                                'accounts.id',
                            ],
                    ],
                'campaign_name' =>
                    [
                        'query_type' => 'default',
                    ],
                'range_date_entered' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
                'start_range_date_entered' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
                'end_range_date_entered' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
                'range_date_modified' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
                'start_range_date_modified' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
                'end_range_date_modified' =>
                    [
                        'query_type' => 'default',
                        'enable_range_search' => true,
                        'is_date_field' => true,
                    ],
            ]
        ];
    }

    /**
     * @return array
     */
    private function getExpectedFields()
    {
        return [
            'first_name',
            'last_name',
            'search_name' =>
                [
                    'first_name',
                    'last_name',
                ],
            'account_name' =>
                [
                    'accounts.name',
                ],
            'phone' =>
                [
                    'phone_mobile',
                    'phone_work',
                    'phone_other',
                    'phone_fax',
                    'assistant_phone',
                ],
            'address_street' =>
                [
                    'primary_address_street',
                    'alt_address_street',
                ],
            'address_city' =>
                [
                    'primary_address_city',
                    'alt_address_city',
                ],
            'address_state' =>
                [
                    'primary_address_state',
                    'alt_address_state',
                ],
            'address_postalcode' =>
                [
                    'primary_address_postalcode',
                    'alt_address_postalcode',
                ],
            'address_country' =>
                [
                    'primary_address_country',
                    'alt_address_country',
                ],
            'current_user_only' =>
                [
                    'assigned_user_id',
                ],
            'lead_source',
            'account_id' =>
                [
                    'accounts.id',
                ],
            'assistant',
            'assigned_user_id',
            'campaign_name',
            'date_entered',
            'created_by',
            'date_modified',
            'modified_user_id',
            'assigned_user_id',
            'modified_by_name',
            'created_by_name',
            'assigned_user_name',
            'assigned_user_name_owner',
        ];
    }

    /**
     * @param $mockModule
     * @return m\MockInterface
     */
    private function getMockParser($mockModule)
    {
        $mockParser = m::mock('ParserSearchFields');
        $mockFields = $this->getFields($mockModule);
        $mockParser
            ->shouldReceive('getSearchFields')
            ->once()
            ->with()
            ->andReturn($mockFields);
        return $mockParser;
    }
}
