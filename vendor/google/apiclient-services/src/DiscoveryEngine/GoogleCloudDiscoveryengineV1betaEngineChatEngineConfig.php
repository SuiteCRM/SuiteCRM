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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig extends \Google\Model
{
  protected $agentCreationConfigType = GoogleCloudDiscoveryengineV1betaEngineChatEngineConfigAgentCreationConfig::class;
  protected $agentCreationConfigDataType = '';
  /**
   * @var string
   */
  public $dialogflowAgentToLink;

  /**
   * @param GoogleCloudDiscoveryengineV1betaEngineChatEngineConfigAgentCreationConfig
   */
  public function setAgentCreationConfig(GoogleCloudDiscoveryengineV1betaEngineChatEngineConfigAgentCreationConfig $agentCreationConfig)
  {
    $this->agentCreationConfig = $agentCreationConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1betaEngineChatEngineConfigAgentCreationConfig
   */
  public function getAgentCreationConfig()
  {
    return $this->agentCreationConfig;
  }
  /**
   * @param string
   */
  public function setDialogflowAgentToLink($dialogflowAgentToLink)
  {
    $this->dialogflowAgentToLink = $dialogflowAgentToLink;
  }
  /**
   * @return string
   */
  public function getDialogflowAgentToLink()
  {
    return $this->dialogflowAgentToLink;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1betaEngineChatEngineConfig');
