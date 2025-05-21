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

namespace Google\Service\AndroidPublisher;

class AppRecoveryAction extends \Google\Model
{
  /**
   * @var string
   */
  public $appRecoveryId;
  /**
   * @var string
   */
  public $cancelTime;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $deployTime;
  /**
   * @var string
   */
  public $lastUpdateTime;
  protected $remoteInAppUpdateDataType = RemoteInAppUpdateData::class;
  protected $remoteInAppUpdateDataDataType = '';
  /**
   * @var string
   */
  public $status;
  protected $targetingType = Targeting::class;
  protected $targetingDataType = '';

  /**
   * @param string
   */
  public function setAppRecoveryId($appRecoveryId)
  {
    $this->appRecoveryId = $appRecoveryId;
  }
  /**
   * @return string
   */
  public function getAppRecoveryId()
  {
    return $this->appRecoveryId;
  }
  /**
   * @param string
   */
  public function setCancelTime($cancelTime)
  {
    $this->cancelTime = $cancelTime;
  }
  /**
   * @return string
   */
  public function getCancelTime()
  {
    return $this->cancelTime;
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
   * @param string
   */
  public function setDeployTime($deployTime)
  {
    $this->deployTime = $deployTime;
  }
  /**
   * @return string
   */
  public function getDeployTime()
  {
    return $this->deployTime;
  }
  /**
   * @param string
   */
  public function setLastUpdateTime($lastUpdateTime)
  {
    $this->lastUpdateTime = $lastUpdateTime;
  }
  /**
   * @return string
   */
  public function getLastUpdateTime()
  {
    return $this->lastUpdateTime;
  }
  /**
   * @param RemoteInAppUpdateData
   */
  public function setRemoteInAppUpdateData(RemoteInAppUpdateData $remoteInAppUpdateData)
  {
    $this->remoteInAppUpdateData = $remoteInAppUpdateData;
  }
  /**
   * @return RemoteInAppUpdateData
   */
  public function getRemoteInAppUpdateData()
  {
    return $this->remoteInAppUpdateData;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param Targeting
   */
  public function setTargeting(Targeting $targeting)
  {
    $this->targeting = $targeting;
  }
  /**
   * @return Targeting
   */
  public function getTargeting()
  {
    return $this->targeting;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AppRecoveryAction::class, 'Google_Service_AndroidPublisher_AppRecoveryAction');
