<?php /** @noinspection PhpUnhandledExceptionInspection */

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

use Elasticsearch\Client;
use SuiteCRM\Search\ElasticSearch\ElasticSearchClientBuilder;
use SuiteCRM\Tests\Unit\lib\SuiteCRM\Search\SearchTestAbstract;

class ElasticSearchClientBuilderTest extends SearchTestAbstract
{
    public function testGetClient(): void
    {
        $client = ElasticSearchClientBuilder::getClient();

        self::assertInstanceOf(Client::class, $client);
    }

    public function testLoadConfig(): void
    {
        $builder = new ElasticSearchClientBuilder();
        $config = $this->invokeMethod($builder, 'loadFromFile', [__DIR__ . '/TestData/ElasticsearchServerConfig.json']);
        $expected = [
            [
                'host' => 'foo.com',
                'port' => '9200',
                'scheme' => 'https',
                'user' => 'username',
                'pass' => 'password!#$?*abc'
            ],
            [
                'host' => 'localhost'
            ]
        ];

        self::assertEquals($expected, $config);
    }

    // Tests if the default configs are returned when the config file is not found
    public function testLoadConfigFileNotThere(): void
    {
        $builder = new ElasticSearchClientBuilder();
        $config = $this->invokeMethod($builder, 'loadFromFile', [__DIR__ . '/TestData/NopeNotHere.json']);
        $expected = [
            ['host' => '127.0.0.1']
        ];

        self::assertEquals($expected, $config);
    }

    public function testLoadSugarConfig(): void
    {
        global $sugar_config;

        $sugar_config['search']['ElasticSearch']['host'] = '127.0.0.1';
        $sugar_config['search']['ElasticSearch']['user'] = 'foo';
        $sugar_config['search']['ElasticSearch']['pass'] = 'bar';

        $actual = $this->loadFromSugarConfig();
        $expected = [
            [
                'host' => '127.0.0.1',
                'user' => 'foo',
                'pass' => 'bar'
            ]
        ];

        self::assertEquals($expected, $actual);
    }

    public function testLoadSugarConfig2(): void
    {
        global $sugar_config;

        $sugar_config['search']['ElasticSearch']['host'] = 'localhost';
        $sugar_config['search']['ElasticSearch']['user'] = 'bar';
        $sugar_config['search']['ElasticSearch']['pass'] = '';

        $actual = $this->loadFromSugarConfig();
        $expected = [
            [
                'host' => 'localhost',
                'user' => 'bar',
                'pass' => ''
            ]
        ];

        self::assertEquals($expected, $actual);


    }

    public function testLoadSugarConfig3(): void
    {
        global $sugar_config;

        $sugar_config['search']['ElasticSearch']['host'] = 'www.example.com';
        $sugar_config['search']['ElasticSearch']['user'] = '';
        $sugar_config['search']['ElasticSearch']['pass'] = '';

        $actual = $this->loadFromSugarConfig();

        $expected = [
            ['host' => 'www.example.com']
        ];

        self::assertEquals($expected, $actual);


    }

    private function loadFromSugarConfig()
    {
        $builder = new ElasticSearchClientBuilder();

        return $this->invokeMethod($builder, 'loadFromSugarConfig');
    }

    private function sanitizeHost(array $host)
    {
        $builder = new ElasticSearchClientBuilder();

        return $this->invokeMethod($builder, 'sanitizeHost', [$host]);
    }

    public function testUrlParser1(): void
    {
        $data = [
            'host' => 'www.example.com'
        ];

        $expected = [
            'host' => 'www.example.com'
        ];

        $actual = $this->sanitizeHost($data);

        self::assertEquals($expected, $actual);
    }

    public function testUrlParser2(): void
    {
        $data = [
            'host' => 'https://www.example.com',
            'user' => 'foo'
        ];

        $expected = [
            'host' => 'www.example.com',
            'user' => 'foo',
            'scheme' => 'https'
        ];

        $actual = $this->sanitizeHost($data);

        self::assertEquals($expected, $actual);
    }

    public function testUrlParser3(): void
    {
        $data = [
            'host' => 'https://www.example.com:42',
            'user' => 'foo',
            'pass' => 'bar'
        ];

        $expected = [
            'host' => 'www.example.com',
            'user' => 'foo',
            'pass' => 'bar',
            'scheme' => 'https',
            'port' => 42
        ];

        $actual = $this->sanitizeHost($data);

        self::assertEquals($expected, $actual);
    }

    public function testUrlParser4(): void
    {
        $data = [
            'host' => 'bar:pass@mydomain.server.com:9201',
            'user' => 'foo',
        ];

        $expected = [
            'host' => 'mydomain.server.com',
            'user' => 'bar',
            'pass' => 'pass',
            'port' => 9201
        ];

        $actual = $this->sanitizeHost($data);

        self::assertEquals($expected, $actual);
    }

    public function testUrlParser5(): void
    {
        $data = [
            'user' => 'foo',
        ];

        $this->expectException(InvalidArgumentException::class);
        $this->sanitizeHost($data);
    }

    public function testUrlParser6(): void
    {
        $data = [
            'host' => 0.5,
            'user' => 'foo',
        ];

        $this->expectException(InvalidArgumentException::class);
        $this->sanitizeHost($data);
    }

    public function testUrlParserBadUrls(): void
    {
        $url1 = ['host' => 'http:///example.com'];
        $url2 = ['host' => 'http://:80'];
        $url3 = ['host' => 'http://user@:80'];

        $this->expectException(InvalidArgumentException::class);
        $this->sanitizeHost($url1);

        $this->expectException(InvalidArgumentException::class);
        $this->sanitizeHost($url2);

        $this->expectException(InvalidArgumentException::class);
        $this->sanitizeHost($url3);
    }
}
