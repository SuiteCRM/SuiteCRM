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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo extends \Google\Collection
{
  protected $collection_key = 'teams';
  /**
   * @var string
   */
  public $agentId;
  /**
   * @var string
   */
  public $agentType;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $dispositionCode;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $team;
  /**
   * @var string[]
   */
  public $teams;

  /**
   * @param string
   */
  public function setAgentId($agentId)
  {
    $this->agentId = $agentId;
  }
  /**
   * @return string
   */
  public function getAgentId()
  {
    return $this->agentId;
  }
  /**
   * @param string
   */
  public function setAgentType($agentType)
  {
    $this->agentType = $agentType;
  }
  /**
   * @return string
   */
  public function getAgentType()
  {
    return $this->agentType;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setDispositionCode($dispositionCode)
  {
    $this->dispositionCode = $dispositionCode;
  }
  /**
   * @return string
   */
  public function getDispositionCode()
  {
    return $this->dispositionCode;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setTeam($team)
  {
    $this->team = $team;
  }
  /**
   * @return string
   */
  public function getTeam()
  {
    return $this->team;
  }
  /**
   * @param string[]
   */
  public function setTeams($teams)
  {
    $this->teams = $teams;
  }
  /**
   * @return string[]
   */
  public function getTeams()
  {
    return $this->teams;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo');
