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

namespace Google\Service\Eventarc;

class GoogleCloudEventarcV1PipelineDestination extends \Google\Model
{
  protected $authenticationConfigType = GoogleCloudEventarcV1PipelineDestinationAuthenticationConfig::class;
  protected $authenticationConfigDataType = '';
  protected $httpEndpointType = GoogleCloudEventarcV1PipelineDestinationHttpEndpoint::class;
  protected $httpEndpointDataType = '';
  /**
   * @var string
   */
  public $messageBus;
  protected $networkConfigType = GoogleCloudEventarcV1PipelineDestinationNetworkConfig::class;
  protected $networkConfigDataType = '';
  protected $outputPayloadFormatType = GoogleCloudEventarcV1PipelineMessagePayloadFormat::class;
  protected $outputPayloadFormatDataType = '';
  /**
   * @var string
   */
  public $topic;
  /**
   * @var string
   */
  public $workflow;

  /**
   * @param GoogleCloudEventarcV1PipelineDestinationAuthenticationConfig
   */
  public function setAuthenticationConfig(GoogleCloudEventarcV1PipelineDestinationAuthenticationConfig $authenticationConfig)
  {
    $this->authenticationConfig = $authenticationConfig;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineDestinationAuthenticationConfig
   */
  public function getAuthenticationConfig()
  {
    return $this->authenticationConfig;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineDestinationHttpEndpoint
   */
  public function setHttpEndpoint(GoogleCloudEventarcV1PipelineDestinationHttpEndpoint $httpEndpoint)
  {
    $this->httpEndpoint = $httpEndpoint;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineDestinationHttpEndpoint
   */
  public function getHttpEndpoint()
  {
    return $this->httpEndpoint;
  }
  /**
   * @param string
   */
  public function setMessageBus($messageBus)
  {
    $this->messageBus = $messageBus;
  }
  /**
   * @return string
   */
  public function getMessageBus()
  {
    return $this->messageBus;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineDestinationNetworkConfig
   */
  public function setNetworkConfig(GoogleCloudEventarcV1PipelineDestinationNetworkConfig $networkConfig)
  {
    $this->networkConfig = $networkConfig;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineDestinationNetworkConfig
   */
  public function getNetworkConfig()
  {
    return $this->networkConfig;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineMessagePayloadFormat
   */
  public function setOutputPayloadFormat(GoogleCloudEventarcV1PipelineMessagePayloadFormat $outputPayloadFormat)
  {
    $this->outputPayloadFormat = $outputPayloadFormat;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineMessagePayloadFormat
   */
  public function getOutputPayloadFormat()
  {
    return $this->outputPayloadFormat;
  }
  /**
   * @param string
   */
  public function setTopic($topic)
  {
    $this->topic = $topic;
  }
  /**
   * @return string
   */
  public function getTopic()
  {
    return $this->topic;
  }
  /**
   * @param string
   */
  public function setWorkflow($workflow)
  {
    $this->workflow = $workflow;
  }
  /**
   * @return string
   */
  public function getWorkflow()
  {
    return $this->workflow;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudEventarcV1PipelineDestination::class, 'Google_Service_Eventarc_GoogleCloudEventarcV1PipelineDestination');
