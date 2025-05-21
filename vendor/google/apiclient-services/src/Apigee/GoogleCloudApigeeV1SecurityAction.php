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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1SecurityAction extends \Google\Collection
{
  protected $collection_key = 'apiProxies';
  protected $allowType = GoogleCloudApigeeV1SecurityActionAllow::class;
  protected $allowDataType = '';
  /**
   * @var string[]
   */
  public $apiProxies;
  protected $conditionConfigType = GoogleCloudApigeeV1SecurityActionConditionConfig::class;
  protected $conditionConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $denyType = GoogleCloudApigeeV1SecurityActionDeny::class;
  protected $denyDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $expireTime;
  protected $flagType = GoogleCloudApigeeV1SecurityActionFlag::class;
  protected $flagDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $ttl;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudApigeeV1SecurityActionAllow
   */
  public function setAllow(GoogleCloudApigeeV1SecurityActionAllow $allow)
  {
    $this->allow = $allow;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityActionAllow
   */
  public function getAllow()
  {
    return $this->allow;
  }
  /**
   * @param string[]
   */
  public function setApiProxies($apiProxies)
  {
    $this->apiProxies = $apiProxies;
  }
  /**
   * @return string[]
   */
  public function getApiProxies()
  {
    return $this->apiProxies;
  }
  /**
   * @param GoogleCloudApigeeV1SecurityActionConditionConfig
   */
  public function setConditionConfig(GoogleCloudApigeeV1SecurityActionConditionConfig $conditionConfig)
  {
    $this->conditionConfig = $conditionConfig;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityActionConditionConfig
   */
  public function getConditionConfig()
  {
    return $this->conditionConfig;
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
   * @param GoogleCloudApigeeV1SecurityActionDeny
   */
  public function setDeny(GoogleCloudApigeeV1SecurityActionDeny $deny)
  {
    $this->deny = $deny;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityActionDeny
   */
  public function getDeny()
  {
    return $this->deny;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
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
   * @param GoogleCloudApigeeV1SecurityActionFlag
   */
  public function setFlag(GoogleCloudApigeeV1SecurityActionFlag $flag)
  {
    $this->flag = $flag;
  }
  /**
   * @return GoogleCloudApigeeV1SecurityActionFlag
   */
  public function getFlag()
  {
    return $this->flag;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
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
class_alias(GoogleCloudApigeeV1SecurityAction::class, 'Google_Service_Apigee_GoogleCloudApigeeV1SecurityAction');
