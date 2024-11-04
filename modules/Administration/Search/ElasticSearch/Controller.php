<?php
/**
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

namespace SuiteCRM\Modules\Administration\Search\ElasticSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use BeanFactory;
use Configurator;
use Elasticsearch\ClientBuilder;
use Elasticsearch\Common\Exceptions\BadRequest400Exception;
use Exception;
use Scheduler;
use SchedulersJob;
use SugarJobQueue;
use SuiteCRM\Modules\Administration\Search\MVC\Controller as AbstractController;
use SuiteCRM\Search\ElasticSearch\ElasticSearchClientBuilder;
use SuiteCRM\Search\ElasticSearch\ElasticSearchIndexer;
use Throwable;

require_once __DIR__ . '/../../../Configurator/Configurator.php';
require_once __DIR__ . '/../../../SchedulersJobs/SchedulersJob.php';
require_once __DIR__ . '/../../../../include/SugarQueue/SugarJobQueue.php';

/**
 * Class Controller handles the actions for the Elasticsearch settings.
 */
class Controller extends AbstractController
{
    /**
     * ElasticSearchSettingsController constructor.
     */
    public function __construct()
    {
        parent::__construct(new View());
    }

    /**
     * Shows the view.
     */
    public function display(): void
    {
        $this->view->getTemplate()->assign('schedulers', $this->getSchedulers());
        parent::display();
    }

    /**
     * Saves the configuration getting data from POST.
     */
    public function doSaveConfig()
    {
        $enabled = filter_input(INPUT_POST, 'enabled', FILTER_VALIDATE_BOOLEAN);
        $host = filter_input(INPUT_POST, 'host');
        $user = filter_input(INPUT_POST, 'user');
        $pass = filter_input(INPUT_POST, 'pass');

        $enabled = (bool) (int) $enabled;

        $cfg = new Configurator();

        $cfg->config['search']['ElasticSearch']['enabled'] = $enabled;
        $cfg->config['search']['ElasticSearch']['host'] = $host;
        $cfg->config['search']['ElasticSearch']['user'] = $user;
        $cfg->config['search']['ElasticSearch']['pass'] = $pass;

        $cfg->saveConfig();

        if ($this->isAjax()) {
            $this->yieldJson(['status' => 'success']);
        }

        $this->redirect('index.php?module=Administration&action=index');
    }

    /**
     * Test the connection with the Elasticsearch and returns a json.
     */
    public function doTestConnection()
    {
        $return = [];
        $input = INPUT_POST;

        $host = filter_input($input, 'host');
        $user = filter_input($input, 'user');
        $pass = filter_input($input, 'pass');

        try {
            $config = [
                ElasticSearchClientBuilder::sanitizeHost([
                    'host' => $host,
                    'user' => $user,
                    'pass' => $pass,
                ]),
            ];

            $client = ClientBuilder::create()->setHosts($config)->build();

            $indexer = new ElasticSearchIndexer($client);

            $return = ['status' => 'fail', 'request' => $config[0],];

            $info = $client->info();
            $time = $indexer->ping();

            $return['status'] = 'success';
            $return['ping'] = $time;
            $return['info'] = $info;
        } /** @noinspection PhpRedundantCatchClauseInspection */
        catch (BadRequest400Exception $exception) {
            $error = json_decode($exception->getMessage());
            $return['error'] = $error->error->reason;
            $return['errorDetails'] = $error;
        } catch (Exception $exception) {
            $return['error'] = $exception->getMessage();
            $return['errorType'] = get_class($exception);
        } catch (Throwable $throwable) {
            $return['error'] = $throwable->getMessage();
            $return['errorType'] = get_class($throwable);
        }

        $this->yieldJson($return);
    }

    /**
     * Schedules a full indexing.
     */
    public function doFullIndex()
    {
        $this->scheduleIndex(false);
    }

    /**
     * Schedules a partial indexing.
     */
    public function doPartialIndex()
    {
        $this->scheduleIndex(true);
    }

    /**
     * Returns all the Elasticsearch-related scheduler jobs.
     *
     * @return Scheduler[]|null
     */
    public function getSchedulers()
    {
        $where = "schedulers.job='function::runElasticSearchIndexerScheduler'";
        /** @var Scheduler[]|null $schedulers */
        $schedulers = BeanFactory::getBean('Schedulers')->get_full_list(null, $where);

        foreach ($schedulers as &$scheduler) {
            $scheduler->check_date_relationships_load();
        }
        unset($scheduler);

        return $schedulers;
    }

    /**
     * Schedules an indexing job.
     *
     * @param bool $partial
     */
    protected function scheduleIndex($partial)
    {
        $job = new SchedulersJob();

        $job->name = 'Index requested by an administrator';
        $job->target = 'function::runElasticSearchIndexerScheduler';
        $job->data = json_encode(['partial' => $partial]);
        $job->assigned_user_id = 1;

        $queue = new SugarJobQueue();
        /** @noinspection PhpParamsInspection */
        $queue->submitJob($job);

        $this->yieldJson(['status' => 'success']);
    }
}
