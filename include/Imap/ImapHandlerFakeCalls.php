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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

// describes the face imap function return values for each function calls with a specific parameters in every test screnario.
return $calls = [

    // this case only for unit testing:
    'testCaseExample' => [],

    'testSettingsOk' => [
        'isAvailable' => [
            ['args' => null, 'return' => [true]],
        ],
        'setTimeout' => [
            ['args' => [1, 60], 'return' => [true]],
            ['args' => [2, 60], 'return' => [true]],
            ['args' => [3, 60], 'return' => [true]],
            ['args' => [1, 15], 'return' => [true]],
            ['args' => [2, 15], 'return' => [true]],
            ['args' => [3, 15], 'return' => [true]],
        ],
        'getErrors' => [
            ['args' => null, 'return' => [false]]
        ],
        'open' => [
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert/secure}INBOX', 'testuser_name', 'testuser_pass', 0, 0, []],
                'return' => [function () {
                    $ret = fopen('fakeImapResource', 'wb+'); // <-- create and return a fake resource for InboundEmail test usages
                    if (!is_resource($ret)) {
                        throw new Exception('Imap fake needs a resource to return (check the file permisson - 1)');
                    }
                    return $ret;
                }],
            ],
        ],
        'getLastError' => [
            ['args' => null, 'return' => [false]],
        ],
        'getAlerts' => [
            ['args' => null, 'return' => [false]],
        ],
        'getConnection' => [],
    ],


    'testSettingsWrongUser' => [
        'isAvailable' => [['args' => null, 'return' => [true]]],
        'setTimeout' => [
            ['args' => [1, 60], 'return' => [true]],
            ['args' => [2, 60], 'return' => [true]],
            ['args' => [3, 60], 'return' => [true]],
            ['args' => [1, 15], 'return' => [true]],
            ['args' => [2, 15], 'return' => [true]],
            ['args' => [3, 15], 'return' => [true]],
        ],
        'getErrors' => [
            ['args' => null, 'return' => [["Can't open mailbox {imap.gmail.com:993\/service=imap\/ssl\/tls\/validate-cert\/secure}INBOX: invalid remote specification"]]],
        ],
        'open' => [
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/validate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/tls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/novalidate-cert}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/notls}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl/secure}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap/ssl}INBOX', 'testuser_name_wrong', 'testuser_pass', 0, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],

            [
                'args' => ['{imap.gmail.com:993/service=imap}INBOX', 'testuser_name_wrong', 'testuser_pass', 32768, 0, []],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap}INBOX', 'testuser_name_wrong', 'testuser_pass', 32768, 0, ['DISABLE_AUTHENTICATOR' => 'GSSAPI']],
                'return' => [false],
            ],
            [
                'args' => ['{imap.gmail.com:993/service=imap}INBOX', 'testuser_name_wrong', 'testuser_pass', 32768, 0, ['DISABLE_AUTHENTICATOR' => 'NTLM']],
                'return' => [false],
            ],
        ],
        'getLastError' => [['args' => null, 'return' => ["Can't open mailbox {imap.gmail.com:993/service=imap/ssl/tls/validate-cert/secure}INBOX: invalid remote specification"]]],
        'getAlerts' => [['args' => null, 'return' => [false]]],
        'getConnection' => [['args' => null, 'return' => [false]]],
//        'close' => [
//            ['args' => null, 'return' => [function() {
//                    if (file_exists('fakeImapResource')) {
//                        unlink('fakeImapResource');
//                    }
//                    return false; // <-- when ImapHandlerFake::close() called, pass back a "FALSE" as failed
//                }]],
//        ],
//    // ... add more possible calls here...
    ],
];
