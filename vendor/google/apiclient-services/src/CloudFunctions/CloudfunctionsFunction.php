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

namespace Google\Service\CloudFunctions;

class CloudfunctionsFunction extends \Google\Collection
{
  protected $collection_key = 'stateMessages';
  protected $buildConfigType = BuildConfig::class;
  protected $buildConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $environment;
  protected $eventTriggerType = EventTrigger::class;
  protected $eventTriggerDataType = '';
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $serviceConfigType = ServiceConfig::class;
  protected $serviceConfigDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $stateMessagesType = GoogleCloudFunctionsV2StateMessage::class;
  protected $stateMessagesDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;
  protected $upgradeInfoType = UpgradeInfo::class;
  protected $upgradeInfoDataType = '';
  /**
   * @var string
   */
  public $url;

  /**
   * @param BuildConfig
   */
  public function setBuildConfig(BuildConfig $buildConfig)
  {
    $this->buildConfig = $buildConfig;
  }
  /**
   * @return BuildConfig
   */
  public function getBuildConfig()
  {
    return $this->buildConfig;
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
  public function setEnvironment($environment)
  {
    $this->environment = $environment;
  }
  /**
   * @return string
   */
  public function getEnvironment()
  {
    return $this->environment;
  }
  /**
   * @param EventTrigger
   */
  public function setEventTrigger(EventTrigger $eventTrigger)
  {
    $this->eventTrigger = $eventTrigger;
  }
  /**
   * @return EventTrigger
   */
  public function getEventTrigger()
  {
    return $this->eventTrigger;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
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
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param ServiceConfig
   */
  public function setServiceConfig(ServiceConfig $serviceConfig)
  {
    $this->serviceConfig = $serviceConfig;
  }
  /**
   * @return ServiceConfig
   */
  public function getServiceConfig()
  {
    return $this->serviceConfig;
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
   * @param GoogleCloudFunctionsV2StateMessage[]
   */
  public function setStateMessages($stateMessages)
  {
    $this->stateMessages = $stateMessages;
  }
  /**
   * @return GoogleCloudFunctionsV2StateMessage[]
   */
  public function getStateMessages()
  {
    return $this->stateMessages;
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
  /**
   * @param UpgradeInfo
   */
  public function setUpgradeInfo(UpgradeInfo $upgradeInfo)
  {
    $this->upgradeInfo = $upgradeInfo;
  }
  /**
   * @return UpgradeInfo
   */
  public function getUpgradeInfo()
  {
    return $this->upgradeInfo;
  }
  /**
   * @param string
   */
  public function setUrl($url)
  {
    $this->url = $url;
  }
  /**
   * @return string
   */
  public function getUrl()
  {
    return $this->url;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudfunctionsFunction::class, 'Google_Service_CloudFunctions_CloudfunctionsFunction');
