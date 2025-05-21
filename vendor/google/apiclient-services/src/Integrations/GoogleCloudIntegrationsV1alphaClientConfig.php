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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaClientConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $billingType;
  /**
   * @var string
   */
  public $clientState;
  protected $cloudKmsConfigType = GoogleCloudIntegrationsV1alphaCloudKmsConfig::class;
  protected $cloudKmsConfigDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var bool
   */
  public $enableInternalIp;
  /**
   * @var bool
   */
  public $enableVariableMasking;
  /**
   * @var string
   */
  public $id;
  /**
   * @var bool
   */
  public $isGmek;
  /**
   * @var string
   */
  public $p4ServiceAccount;
  /**
   * @var string
   */
  public $projectId;
  /**
   * @var string
   */
  public $region;
  /**
   * @var string
   */
  public $runAsServiceAccount;

  /**
   * @param string
   */
  public function setBillingType($billingType)
  {
    $this->billingType = $billingType;
  }
  /**
   * @return string
   */
  public function getBillingType()
  {
    return $this->billingType;
  }
  /**
   * @param string
   */
  public function setClientState($clientState)
  {
    $this->clientState = $clientState;
  }
  /**
   * @return string
   */
  public function getClientState()
  {
    return $this->clientState;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaCloudKmsConfig
   */
  public function setCloudKmsConfig(GoogleCloudIntegrationsV1alphaCloudKmsConfig $cloudKmsConfig)
  {
    $this->cloudKmsConfig = $cloudKmsConfig;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaCloudKmsConfig
   */
  public function getCloudKmsConfig()
  {
    return $this->cloudKmsConfig;
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
   * @param bool
   */
  public function setEnableInternalIp($enableInternalIp)
  {
    $this->enableInternalIp = $enableInternalIp;
  }
  /**
   * @return bool
   */
  public function getEnableInternalIp()
  {
    return $this->enableInternalIp;
  }
  /**
   * @param bool
   */
  public function setEnableVariableMasking($enableVariableMasking)
  {
    $this->enableVariableMasking = $enableVariableMasking;
  }
  /**
   * @return bool
   */
  public function getEnableVariableMasking()
  {
    return $this->enableVariableMasking;
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
   * @param bool
   */
  public function setIsGmek($isGmek)
  {
    $this->isGmek = $isGmek;
  }
  /**
   * @return bool
   */
  public function getIsGmek()
  {
    return $this->isGmek;
  }
  /**
   * @param string
   */
  public function setP4ServiceAccount($p4ServiceAccount)
  {
    $this->p4ServiceAccount = $p4ServiceAccount;
  }
  /**
   * @return string
   */
  public function getP4ServiceAccount()
  {
    return $this->p4ServiceAccount;
  }
  /**
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param string
   */
  public function setRegion($region)
  {
    $this->region = $region;
  }
  /**
   * @return string
   */
  public function getRegion()
  {
    return $this->region;
  }
  /**
   * @param string
   */
  public function setRunAsServiceAccount($runAsServiceAccount)
  {
    $this->runAsServiceAccount = $runAsServiceAccount;
  }
  /**
   * @return string
   */
  public function getRunAsServiceAccount()
  {
    return $this->runAsServiceAccount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaClientConfig::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaClientConfig');
