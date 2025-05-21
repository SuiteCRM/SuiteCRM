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

class GoogleCloudIntegrationsV1alphaProvisionClientRequest extends \Google\Model
{
  protected $cloudKmsConfigType = GoogleCloudIntegrationsV1alphaCloudKmsConfig::class;
  protected $cloudKmsConfigDataType = '';
  /**
   * @var bool
   */
  public $createSampleWorkflows;
  /**
   * @var bool
   */
  public $provisionGmek;
  /**
   * @var string
   */
  public $runAsServiceAccount;
  /**
   * @var bool
   */
  public $skipCpProvision;

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
   * @param bool
   */
  public function setCreateSampleWorkflows($createSampleWorkflows)
  {
    $this->createSampleWorkflows = $createSampleWorkflows;
  }
  /**
   * @return bool
   */
  public function getCreateSampleWorkflows()
  {
    return $this->createSampleWorkflows;
  }
  /**
   * @param bool
   */
  public function setProvisionGmek($provisionGmek)
  {
    $this->provisionGmek = $provisionGmek;
  }
  /**
   * @return bool
   */
  public function getProvisionGmek()
  {
    return $this->provisionGmek;
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
  /**
   * @param bool
   */
  public function setSkipCpProvision($skipCpProvision)
  {
    $this->skipCpProvision = $skipCpProvision;
  }
  /**
   * @return bool
   */
  public function getSkipCpProvision()
  {
    return $this->skipCpProvision;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaProvisionClientRequest::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaProvisionClientRequest');
