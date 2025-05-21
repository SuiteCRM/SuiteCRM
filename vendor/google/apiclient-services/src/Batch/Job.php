<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Batch;

class Job extends \Google\Collection
{
  protected $collection_key = 'taskGroups';
  protected $allocationPolicyType = AllocationPolicy::class;
  protected $allocationPolicyDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $labels;
  protected $logsPolicyType = LogsPolicy::class;
  protected $logsPolicyDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $notificationsType = JobNotification::class;
  protected $notificationsDataType = 'array';
  /**
   * @var string
   */
  public $priority;
  protected $statusType = JobStatus::class;
  protected $statusDataType = '';
  protected $taskGroupsType = TaskGroup::class;
  protected $taskGroupsDataType = 'array';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param AllocationPolicy
   */
  public function setAllocationPolicy(AllocationPolicy $allocationPolicy)
  {
    $this->allocationPolicy = $allocationPolicy;
  }
  /**
   * @return AllocationPolicy
   */
  public function getAllocationPolicy()
  {
    return $this->allocationPolicy;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param LogsPolicy
   */
  public function setLogsPolicy(LogsPolicy $logsPolicy)
  {
    $this->logsPolicy = $logsPolicy;
  }
  /**
   * @return LogsPolicy
   */
  public function getLogsPolicy()
  {
    return $this->logsPolicy;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param JobNotification[]
   */
  public function setNotifications($notifications)
  {
    $this->notifications = $notifications;
  }
  /**
   * @return JobNotification[]
   */
  public function getNotifications()
  {
    return $this->notifications;
  }
  /**
   * @param string
   */
  public function setPriority($priority)
  {
    $this->priority = $priority;
  }
  /**
   * @return string
   */
  public function getPriority()
  {
    return $this->priority;
  }
  /**
   * @param JobStatus
   */
  public function setStatus(JobStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return JobStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param TaskGroup[]
   */
  public function setTaskGroups($taskGroups)
  {
    $this->taskGroups = $taskGroups;
  }
  /**
   * @return TaskGroup[]
   */
  public function getTaskGroups()
  {
    return $this->taskGroups;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Job::class, 'Google_Service_Batch_Job');
