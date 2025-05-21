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

namespace Google\Service\AndroidManagement;

class MigrationToken extends \Google\Model
{
  /**
   * @var string
   */
  public $additionalData;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $device;
  /**
   * @var string
   */
  public $deviceId;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $managementMode;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $policy;
  /**
   * @var string
   */
  public $ttl;
  /**
   * @var string
   */
  public $userId;
  /**
   * @var string
   */
  public $value;

  /**
   * @param string
   */
  public function setAdditionalData($additionalData)
  {
    $this->additionalData = $additionalData;
  }
  /**
   * @return string
   */
  public function getAdditionalData()
  {
    return $this->additionalData;
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
  public function setDevice($device)
  {
    $this->device = $device;
  }
  /**
   * @return string
   */
  public function getDevice()
  {
    return $this->device;
  }
  /**
   * @param string
   */
  public function setDeviceId($deviceId)
  {
    $this->deviceId = $deviceId;
  }
  /**
   * @return string
   */
  public function getDeviceId()
  {
    return $this->deviceId;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
  }
  /**
   * @param string
   */
  public function setManagementMode($managementMode)
  {
    $this->managementMode = $managementMode;
  }
  /**
   * @return string
   */
  public function getManagementMode()
  {
    return $this->managementMode;
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
   * @param string
   */
  public function setPolicy($policy)
  {
    $this->policy = $policy;
  }
  /**
   * @return string
   */
  public function getPolicy()
  {
    return $this->policy;
  }
  /**
   * @param string
   */
  public function setTtl($ttl)
  {
    $this->ttl = $ttl;
  }
  /**
   * @return string
   */
  public function getTtl()
  {
    return $this->ttl;
  }
  /**
   * @param string
   */
  public function setUserId($userId)
  {
    $this->userId = $userId;
  }
  /**
   * @return string
   */
  public function getUserId()
  {
    return $this->userId;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MigrationToken::class, 'Google_Service_AndroidManagement_MigrationToken');
