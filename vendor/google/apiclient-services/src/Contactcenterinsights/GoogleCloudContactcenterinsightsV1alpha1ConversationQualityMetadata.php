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

class GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadata extends \Google\Collection
{
  protected $collection_key = 'agentInfo';
  protected $agentInfoType = GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo::class;
  protected $agentInfoDataType = 'array';
  /**
   * @var int
   */
  public $customerSatisfactionRating;
  /**
   * @var string
   */
  public $menuPath;
  /**
   * @var string
   */
  public $waitDuration;

  /**
   * @param GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo[]
   */
  public function setAgentInfo($agentInfo)
  {
    $this->agentInfo = $agentInfo;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadataAgentInfo[]
   */
  public function getAgentInfo()
  {
    return $this->agentInfo;
  }
  /**
   * @param int
   */
  public function setCustomerSatisfactionRating($customerSatisfactionRating)
  {
    $this->customerSatisfactionRating = $customerSatisfactionRating;
  }
  /**
   * @return int
   */
  public function getCustomerSatisfactionRating()
  {
    return $this->customerSatisfactionRating;
  }
  /**
   * @param string
   */
  public function setMenuPath($menuPath)
  {
    $this->menuPath = $menuPath;
  }
  /**
   * @return string
   */
  public function getMenuPath()
  {
    return $this->menuPath;
  }
  /**
   * @param string
   */
  public function setWaitDuration($waitDuration)
  {
    $this->waitDuration = $waitDuration;
  }
  /**
   * @return string
   */
  public function getWaitDuration()
  {
    return $this->waitDuration;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadata::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1alpha1ConversationQualityMetadata');
