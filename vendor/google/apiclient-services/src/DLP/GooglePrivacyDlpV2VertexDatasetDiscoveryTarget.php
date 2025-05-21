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

namespace Google\Service\DLP;

class GooglePrivacyDlpV2VertexDatasetDiscoveryTarget extends \Google\Model
{
  protected $conditionsType = GooglePrivacyDlpV2DiscoveryVertexDatasetConditions::class;
  protected $conditionsDataType = '';
  protected $disabledType = GooglePrivacyDlpV2Disabled::class;
  protected $disabledDataType = '';
  protected $filterType = GooglePrivacyDlpV2DiscoveryVertexDatasetFilter::class;
  protected $filterDataType = '';
  protected $generationCadenceType = GooglePrivacyDlpV2DiscoveryVertexDatasetGenerationCadence::class;
  protected $generationCadenceDataType = '';

  /**
   * @param GooglePrivacyDlpV2DiscoveryVertexDatasetConditions
   */
  public function setConditions(GooglePrivacyDlpV2DiscoveryVertexDatasetConditions $conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryVertexDatasetConditions
   */
  public function getConditions()
  {
    return $this->conditions;
  }
  /**
   * @param GooglePrivacyDlpV2Disabled
   */
  public function setDisabled(GooglePrivacyDlpV2Disabled $disabled)
  {
    $this->disabled = $disabled;
  }
  /**
   * @return GooglePrivacyDlpV2Disabled
   */
  public function getDisabled()
  {
    return $this->disabled;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryVertexDatasetFilter
   */
  public function setFilter(GooglePrivacyDlpV2DiscoveryVertexDatasetFilter $filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryVertexDatasetFilter
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryVertexDatasetGenerationCadence
   */
  public function setGenerationCadence(GooglePrivacyDlpV2DiscoveryVertexDatasetGenerationCadence $generationCadence)
  {
    $this->generationCadence = $generationCadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryVertexDatasetGenerationCadence
   */
  public function getGenerationCadence()
  {
    return $this->generationCadence;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2VertexDatasetDiscoveryTarget::class, 'Google_Service_DLP_GooglePrivacyDlpV2VertexDatasetDiscoveryTarget');
