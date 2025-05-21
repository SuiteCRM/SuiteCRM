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

class GoogleCloudIntegrationsV1alphaTestIntegrationsRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $clientId;
  /**
   * @var array[]
   */
  public $configParameters;
  /**
   * @var string
   */
  public $deadlineSecondsTime;
  protected $inputParametersType = GoogleCloudIntegrationsV1alphaValueType::class;
  protected $inputParametersDataType = 'map';
  protected $integrationVersionType = GoogleCloudIntegrationsV1alphaIntegrationVersion::class;
  protected $integrationVersionDataType = '';
  protected $parametersType = EnterpriseCrmFrontendsEventbusProtoEventParameters::class;
  protected $parametersDataType = '';
  /**
   * @var bool
   */
  public $testMode;
  /**
   * @var string
   */
  public $triggerId;

  /**
   * @param string
   */
  public function setClientId($clientId)
  {
    $this->clientId = $clientId;
  }
  /**
   * @return string
   */
  public function getClientId()
  {
    return $this->clientId;
  }
  /**
   * @param array[]
   */
  public function setConfigParameters($configParameters)
  {
    $this->configParameters = $configParameters;
  }
  /**
   * @return array[]
   */
  public function getConfigParameters()
  {
    return $this->configParameters;
  }
  /**
   * @param string
   */
  public function setDeadlineSecondsTime($deadlineSecondsTime)
  {
    $this->deadlineSecondsTime = $deadlineSecondsTime;
  }
  /**
   * @return string
   */
  public function getDeadlineSecondsTime()
  {
    return $this->deadlineSecondsTime;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaValueType[]
   */
  public function setInputParameters($inputParameters)
  {
    $this->inputParameters = $inputParameters;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaValueType[]
   */
  public function getInputParameters()
  {
    return $this->inputParameters;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaIntegrationVersion
   */
  public function setIntegrationVersion(GoogleCloudIntegrationsV1alphaIntegrationVersion $integrationVersion)
  {
    $this->integrationVersion = $integrationVersion;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaIntegrationVersion
   */
  public function getIntegrationVersion()
  {
    return $this->integrationVersion;
  }
  /**
   * @param EnterpriseCrmFrontendsEventbusProtoEventParameters
   */
  public function setParameters(EnterpriseCrmFrontendsEventbusProtoEventParameters $parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return EnterpriseCrmFrontendsEventbusProtoEventParameters
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param bool
   */
  public function setTestMode($testMode)
  {
    $this->testMode = $testMode;
  }
  /**
   * @return bool
   */
  public function getTestMode()
  {
    return $this->testMode;
  }
  /**
   * @param string
   */
  public function setTriggerId($triggerId)
  {
    $this->triggerId = $triggerId;
  }
  /**
   * @return string
   */
  public function getTriggerId()
  {
    return $this->triggerId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaTestIntegrationsRequest::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaTestIntegrationsRequest');
