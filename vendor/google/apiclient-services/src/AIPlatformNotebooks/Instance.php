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

namespace Google\Service\AIPlatformNotebooks;

class Instance extends \Google\Collection
{
  protected $collection_key = 'upgradeHistory';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creator;
  /**
   * @var bool
   */
  public $disableProxyAccess;
  /**
   * @var bool
   */
  public $enableThirdPartyIdentity;
  protected $gceSetupType = GceSetup::class;
  protected $gceSetupDataType = '';
  /**
   * @var string[]
   */
  public $healthInfo;
  /**
   * @var string
   */
  public $healthState;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $instanceOwners;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $proxyUri;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $thirdPartyProxyUrl;
  /**
   * @var string
   */
  public $updateTime;
  protected $upgradeHistoryType = UpgradeHistoryEntry::class;
  protected $upgradeHistoryDataType = 'array';

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
  public function setCreator($creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return string
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param bool
   */
  public function setDisableProxyAccess($disableProxyAccess)
  {
    $this->disableProxyAccess = $disableProxyAccess;
  }
  /**
   * @return bool
   */
  public function getDisableProxyAccess()
  {
    return $this->disableProxyAccess;
  }
  /**
   * @param bool
   */
  public function setEnableThirdPartyIdentity($enableThirdPartyIdentity)
  {
    $this->enableThirdPartyIdentity = $enableThirdPartyIdentity;
  }
  /**
   * @return bool
   */
  public function getEnableThirdPartyIdentity()
  {
    return $this->enableThirdPartyIdentity;
  }
  /**
   * @param GceSetup
   */
  public function setGceSetup(GceSetup $gceSetup)
  {
    $this->gceSetup = $gceSetup;
  }
  /**
   * @return GceSetup
   */
  public function getGceSetup()
  {
    return $this->gceSetup;
  }
  /**
   * @param string[]
   */
  public function setHealthInfo($healthInfo)
  {
    $this->healthInfo = $healthInfo;
  }
  /**
   * @return string[]
   */
  public function getHealthInfo()
  {
    return $this->healthInfo;
  }
  /**
   * @param string
   */
  public function setHealthState($healthState)
  {
    $this->healthState = $healthState;
  }
  /**
   * @return string
   */
  public function getHealthState()
  {
    return $this->healthState;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string[]
   */
  public function setInstanceOwners($instanceOwners)
  {
    $this->instanceOwners = $instanceOwners;
  }
  /**
   * @return string[]
   */
  public function getInstanceOwners()
  {
    return $this->instanceOwners;
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
   * @param string
   */
  public function setProxyUri($proxyUri)
  {
    $this->proxyUri = $proxyUri;
  }
  /**
   * @return string
   */
  public function getProxyUri()
  {
    return $this->proxyUri;
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
  public function setThirdPartyProxyUrl($thirdPartyProxyUrl)
  {
    $this->thirdPartyProxyUrl = $thirdPartyProxyUrl;
  }
  /**
   * @return string
   */
  public function getThirdPartyProxyUrl()
  {
    return $this->thirdPartyProxyUrl;
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
   * @param UpgradeHistoryEntry[]
   */
  public function setUpgradeHistory($upgradeHistory)
  {
    $this->upgradeHistory = $upgradeHistory;
  }
  /**
   * @return UpgradeHistoryEntry[]
   */
  public function getUpgradeHistory()
  {
    return $this->upgradeHistory;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Instance::class, 'Google_Service_AIPlatformNotebooks_Instance');
