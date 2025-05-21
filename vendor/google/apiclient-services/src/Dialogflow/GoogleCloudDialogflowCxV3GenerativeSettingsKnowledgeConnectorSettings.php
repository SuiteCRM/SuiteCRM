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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3GenerativeSettingsKnowledgeConnectorSettings extends \Google\Model
{
  /**
   * @var string
   */
  public $agent;
  /**
   * @var string
   */
  public $agentIdentity;
  /**
   * @var string
   */
  public $agentScope;
  /**
   * @var string
   */
  public $business;
  /**
   * @var string
   */
  public $businessDescription;
  /**
   * @var bool
   */
  public $disableDataStoreFallback;

  /**
   * @param string
   */
  public function setAgent($agent)
  {
    $this->agent = $agent;
  }
  /**
   * @return string
   */
  public function getAgent()
  {
    return $this->agent;
  }
  /**
   * @param string
   */
  public function setAgentIdentity($agentIdentity)
  {
    $this->agentIdentity = $agentIdentity;
  }
  /**
   * @return string
   */
  public function getAgentIdentity()
  {
    return $this->agentIdentity;
  }
  /**
   * @param string
   */
  public function setAgentScope($agentScope)
  {
    $this->agentScope = $agentScope;
  }
  /**
   * @return string
   */
  public function getAgentScope()
  {
    return $this->agentScope;
  }
  /**
   * @param string
   */
  public function setBusiness($business)
  {
    $this->business = $business;
  }
  /**
   * @return string
   */
  public function getBusiness()
  {
    return $this->business;
  }
  /**
   * @param string
   */
  public function setBusinessDescription($businessDescription)
  {
    $this->businessDescription = $businessDescription;
  }
  /**
   * @return string
   */
  public function getBusinessDescription()
  {
    return $this->businessDescription;
  }
  /**
   * @param bool
   */
  public function setDisableDataStoreFallback($disableDataStoreFallback)
  {
    $this->disableDataStoreFallback = $disableDataStoreFallback;
  }
  /**
   * @return bool
   */
  public function getDisableDataStoreFallback()
  {
    return $this->disableDataStoreFallback;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3GenerativeSettingsKnowledgeConnectorSettings::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3GenerativeSettingsKnowledgeConnectorSettings');
