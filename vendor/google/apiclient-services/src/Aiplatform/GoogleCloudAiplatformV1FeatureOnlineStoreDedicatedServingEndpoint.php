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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint extends \Google\Model
{
  protected $privateServiceConnectConfigType = GoogleCloudAiplatformV1PrivateServiceConnectConfig::class;
  protected $privateServiceConnectConfigDataType = '';
  /**
   * @var string
   */
  public $publicEndpointDomainName;
  /**
   * @var string
   */
  public $serviceAttachment;

  /**
   * @param GoogleCloudAiplatformV1PrivateServiceConnectConfig
   */
  public function setPrivateServiceConnectConfig(GoogleCloudAiplatformV1PrivateServiceConnectConfig $privateServiceConnectConfig)
  {
    $this->privateServiceConnectConfig = $privateServiceConnectConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1PrivateServiceConnectConfig
   */
  public function getPrivateServiceConnectConfig()
  {
    return $this->privateServiceConnectConfig;
  }
  /**
   * @param string
   */
  public function setPublicEndpointDomainName($publicEndpointDomainName)
  {
    $this->publicEndpointDomainName = $publicEndpointDomainName;
  }
  /**
   * @return string
   */
  public function getPublicEndpointDomainName()
  {
    return $this->publicEndpointDomainName;
  }
  /**
   * @param string
   */
  public function setServiceAttachment($serviceAttachment)
  {
    $this->serviceAttachment = $serviceAttachment;
  }
  /**
   * @return string
   */
  public function getServiceAttachment()
  {
    return $this->serviceAttachment;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureOnlineStoreDedicatedServingEndpoint');
