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

namespace Google\Service\SecurityCommandCenter;

class BackupDisasterRecovery extends \Google\Collection
{
  protected $collection_key = 'policyOptions';
  /**
   * @var string
   */
  public $appliance;
  /**
   * @var string[]
   */
  public $applications;
  /**
   * @var string
   */
  public $backupCreateTime;
  /**
   * @var string
   */
  public $backupTemplate;
  /**
   * @var string
   */
  public $backupType;
  /**
   * @var string
   */
  public $host;
  /**
   * @var string[]
   */
  public $policies;
  /**
   * @var string[]
   */
  public $policyOptions;
  /**
   * @var string
   */
  public $profile;
  /**
   * @var string
   */
  public $storagePool;

  /**
   * @param string
   */
  public function setAppliance($appliance)
  {
    $this->appliance = $appliance;
  }
  /**
   * @return string
   */
  public function getAppliance()
  {
    return $this->appliance;
  }
  /**
   * @param string[]
   */
  public function setApplications($applications)
  {
    $this->applications = $applications;
  }
  /**
   * @return string[]
   */
  public function getApplications()
  {
    return $this->applications;
  }
  /**
   * @param string
   */
  public function setBackupCreateTime($backupCreateTime)
  {
    $this->backupCreateTime = $backupCreateTime;
  }
  /**
   * @return string
   */
  public function getBackupCreateTime()
  {
    return $this->backupCreateTime;
  }
  /**
   * @param string
   */
  public function setBackupTemplate($backupTemplate)
  {
    $this->backupTemplate = $backupTemplate;
  }
  /**
   * @return string
   */
  public function getBackupTemplate()
  {
    return $this->backupTemplate;
  }
  /**
   * @param string
   */
  public function setBackupType($backupType)
  {
    $this->backupType = $backupType;
  }
  /**
   * @return string
   */
  public function getBackupType()
  {
    return $this->backupType;
  }
  /**
   * @param string
   */
  public function setHost($host)
  {
    $this->host = $host;
  }
  /**
   * @return string
   */
  public function getHost()
  {
    return $this->host;
  }
  /**
   * @param string[]
   */
  public function setPolicies($policies)
  {
    $this->policies = $policies;
  }
  /**
   * @return string[]
   */
  public function getPolicies()
  {
    return $this->policies;
  }
  /**
   * @param string[]
   */
  public function setPolicyOptions($policyOptions)
  {
    $this->policyOptions = $policyOptions;
  }
  /**
   * @return string[]
   */
  public function getPolicyOptions()
  {
    return $this->policyOptions;
  }
  /**
   * @param string
   */
  public function setProfile($profile)
  {
    $this->profile = $profile;
  }
  /**
   * @return string
   */
  public function getProfile()
  {
    return $this->profile;
  }
  /**
   * @param string
   */
  public function setStoragePool($storagePool)
  {
    $this->storagePool = $storagePool;
  }
  /**
   * @return string
   */
  public function getStoragePool()
  {
    return $this->storagePool;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BackupDisasterRecovery::class, 'Google_Service_SecurityCommandCenter_BackupDisasterRecovery');
