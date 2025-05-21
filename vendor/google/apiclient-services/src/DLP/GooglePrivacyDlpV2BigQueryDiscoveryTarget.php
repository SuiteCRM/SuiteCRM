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

class GooglePrivacyDlpV2BigQueryDiscoveryTarget extends \Google\Model
{
  protected $cadenceType = GooglePrivacyDlpV2DiscoveryGenerationCadence::class;
  protected $cadenceDataType = '';
  protected $conditionsType = GooglePrivacyDlpV2DiscoveryBigQueryConditions::class;
  protected $conditionsDataType = '';
  protected $disabledType = GooglePrivacyDlpV2Disabled::class;
  protected $disabledDataType = '';
  protected $filterType = GooglePrivacyDlpV2DiscoveryBigQueryFilter::class;
  protected $filterDataType = '';

  /**
   * @param GooglePrivacyDlpV2DiscoveryGenerationCadence
   */
  public function setCadence(GooglePrivacyDlpV2DiscoveryGenerationCadence $cadence)
  {
    $this->cadence = $cadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryGenerationCadence
   */
  public function getCadence()
  {
    return $this->cadence;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryBigQueryConditions
   */
  public function setConditions(GooglePrivacyDlpV2DiscoveryBigQueryConditions $conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryBigQueryConditions
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
   * @param GooglePrivacyDlpV2DiscoveryBigQueryFilter
   */
  public function setFilter(GooglePrivacyDlpV2DiscoveryBigQueryFilter $filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryBigQueryFilter
   */
  public function getFilter()
  {
    return $this->filter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2BigQueryDiscoveryTarget::class, 'Google_Service_DLP_GooglePrivacyDlpV2BigQueryDiscoveryTarget');
