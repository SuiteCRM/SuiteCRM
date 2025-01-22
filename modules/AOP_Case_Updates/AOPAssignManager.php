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

/**
 * Class AOPAssignManager.
 */
#[\AllowDynamicProperties]
class AOPAssignManager
{
    private $ieX = false;
    private $distributionMethod = '';
    private $aopFallback = true;
    private $assignableUsers = array();

    /**
     * AOPAssignManager constructor.
     *
     * @param  InboundEmail|bool $ieX
     */
    public function __construct($ieX = false)
    {
        global $sugar_config;
        $this->ieX = $ieX;
        $inboundDistributionMethod = '';
        if ($ieX) {
            $inboundDistributionMethod = $ieX->get_stored_options('distrib_method', '');
        }
        if ($this->isAOPFallback($inboundDistributionMethod)) {
            $this->distributionMethod = $sugar_config['aop']['distribution_method'];
            $this->aopFallback = true;
        } else {
            $this->distributionMethod = $inboundDistributionMethod;
            $this->aopFallback = false;
        }
        $this->assignableUsers = $this->getAssignableUsers();
    }

    /**
     * @param $distributionMethod
     *
     * @return bool
     */
    private function isAOPFallback($distributionMethod)
    {
        return empty($distributionMethod) || $distributionMethod === 'AOPDefault';
    }

    /**
     * @return mixed
     */
    private function getDistributionOptions()
    {
        global $sugar_config;
        if ($this->aopFallback) {
            $distributionOptions = null;
            if (isset($sugar_config['aop']['distribution_options'])) {
                $distributionOptions = $sugar_config['aop']['distribution_options'];
            } else {
                LoggerManager::getLogger()->warn('$sugar_config[aop][distribution_options] is not defined');
            }

            return $distributionOptions;
        } else {
            return $this->ieX->get_stored_options('distribution_options', '');
        }
    }

    /**
     * @param $roleId
     *
     * @return array
     */
    private function getRoleUsers($roleId)
    {
        require_once 'modules/ACLRoles/ACLRole.php';
        $role = BeanFactory::newBean('ACLRoles');
        $role->retrieve($roleId);
        $role_users = $role->get_linked_beans('users', 'User');
        $r_users = array();
        foreach ($role_users as $role_user) {
            $r_users[$role_user->id] = $role_user->name;
        }

        return $r_users;
    }

    /**
     * @return array
     */
    private function getAssignableUsers()
    {
        if ($this->distributionMethod === 'singleUser') {
            return array();
        }
        $distributionOptions = $this->getDistributionOptions();

        if (empty($distributionOptions)) {
            return array();
        }
        switch ($distributionOptions[0]) {
            case 'security_group':
                if (file_exists('modules/SecurityGroups/SecurityGroup.php')) {
                    require_once 'modules/SecurityGroups/SecurityGroup.php';
                    $security_group = BeanFactory::newBean('SecurityGroups');
                    $security_group->retrieve($distributionOptions[1]);
                    $group_users = $security_group->get_linked_beans('users', 'User');
                    $users = array();
                    $r_users = array();
                    if ($distributionOptions[2] !== '') {
                        $r_users = $this->getRoleUsers($distributionOptions[2]);
                    }
                    foreach ($group_users as $group_user) {
                        if ($distributionOptions[2] !== '' && !isset($r_users[$group_user->id])) {
                            continue;
                        }
                        $users[$group_user->id] = $group_user->name;
                    }
                    break;
                }
            //No Security Group module found - fall through.
            // no break
            case 'role':
                $users = $this->getRoleUsers($distributionOptions[2]);
                break;
            case 'all':
            default:
                $users = get_user_array(false);
                break;
        }

        return $users;
    }

    /**
     * @return string
     */
    private function getSingleUser()
    {
        global $sugar_config;
        if ($this->singleUser) {
            return $this->singleUser;
        }
        if ($this->aopFallback) {
            $this->singleUser = !empty($sugar_config['aop']['distribution_user_id']) ? $sugar_config['aop']['distribution_user_id'] : '';
        } else {
            $this->singleUser = $this->ieX->get_stored_options('distribution_user_id', '');
        }

        return $this->singleUser;
    }

    /**
     * @return array
     */
    private function getLeastBusyCounts()
    {
        if ($this->leastBusyUsers) {
            return $this->leastBusyUsers;
        }
        $db = DBManagerFactory::getInstance();
        $idIn = implode("','", $db->arrayQuote(array_keys($this->assignableUsers)));
        if ($idIn) {
            $idIn = "'".$idIn."'";
        }
        $res = $db->query("SELECT assigned_user_id, COUNT(*) AS c FROM cases WHERE assigned_user_id IN ({$idIn}) AND deleted = 0 GROUP BY assigned_user_id ORDER BY COUNT(*)");
        $this->leastBusyUsers = array();
        while ($row = $db->fetchByAssoc($res)) {
            $this->leastBusyUsers[$row['assigned_user_id']] = $row['c'];
        }

        return $this->leastBusyUsers;
    }

    /**
     * @return mixed
     */
    private function getLeastBusyUser()
    {
        $leastBusyCounts = $this->getLeastBusyCounts();
        asort($leastBusyCounts);

        return array_key_first($leastBusyCounts);
    }

    /**
     * @param $id
     */
    private function updateLeastBusy($id)
    {
        if (!$this->leastBusyUsers) {
            $this->getLeastBusyCounts();
        }
        $this->leastBusyUsers[$id] += 1;
    }

    /**
     * @return mixed
     */
    private function getRandomUser()
    {
        $randKey = array_rand($this->assignableUsers);

        return $this->assignableUsers[$randKey];
    }

    /**
     * @return mixed|string
     */
    public function getNextAssignedUser()
    {
        switch ($this->distributionMethod) {
            case 'singleUser':
                $userId = $this->getSingleUser();
                break;
            case 'roundRobin':
                $userId = $this->getRoundRobinUser();
                $this->setLastRoundRobinUser($userId);
                break;
            case 'leastBusy':
                $userId = $this->getLeastBusyUser();
                $this->updateLeastBusy($userId);
                break;
            case 'random':
                $userId = $this->getRandomUser();
                break;
            default:
                $userId = '';
        }

        return $userId;
    }

    /**
     * @return mixed
     */
    private function getRoundRobinUser()
    {
        $lastUser = [];
        $id = empty($this->ieX) ? '' : $this->ieX->id;
        $file = create_cache_directory('modules/AOP_Case_Updates/Users/').$id.'lastUser.cache.php';
        $lastUserId = '';
        if (isset($_SESSION['AOPLastUser'][$id]) && $_SESSION['AOPLastUser'][$id] !== '') {
            $lastUserId = $_SESSION['AOPLastUser'][$id];
        } elseif (is_file($file)) {
            include $file;
            if (isset($lastUser['User']) && $lastUser['User'] !== '') {
                $lastUserId = $lastUser['User'];
            }
        }
        $users = array_keys($this->assignableUsers);
        $lastOffset = array_search($lastUserId, $users, false);
        $newOffset = count($users) !== 0 ? ($lastOffset + 1) % count($users) : 0;
        if (!empty($users[$newOffset])) {
            return $users[$newOffset];
        }

        return reset($users);
    }

    /**
     * @param $user_id
     *
     * @return bool
     */
    private function setLastRoundRobinUser($user_id)
    {
        $id = empty($this->ieX) ? '' : $this->ieX->id;
        $_SESSION['AOPLastUser'][$id] = $user_id;
        $file = create_cache_directory('modules/AOP_Case_Updates/Users/').$id.'lastUser.cache.php';
        $arrayString = var_export_helper(array('User' => $user_id));
        $content = <<<eoq
<?php
    \$lastUser = {$arrayString};
?>
eoq;
        sugar_file_put_contents($file, $content);

        return true;
    }
}
