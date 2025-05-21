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

class GooglePrivacyDlpV2OtherCloudDiscoveryTarget extends \Google\Model
{
  protected $conditionsType = GooglePrivacyDlpV2DiscoveryOtherCloudConditions::class;
  protected $conditionsDataType = '';
  protected $dataSourceTypeType = GooglePrivacyDlpV2DataSourceType::class;
  protected $dataSourceTypeDataType = '';
  protected $disabledType = GooglePrivacyDlpV2Disabled::class;
  protected $disabledDataType = '';
  protected $filterType = GooglePrivacyDlpV2DiscoveryOtherCloudFilter::class;
  protected $filterDataType = '';
  protected $generationCadenceType = GooglePrivacyDlpV2DiscoveryOtherCloudGenerationCadence::class;
  protected $generationCadenceDataType = '';

  /**
   * @param GooglePrivacyDlpV2DiscoveryOtherCloudConditions
   */
  public function setConditions(GooglePrivacyDlpV2DiscoveryOtherCloudConditions $conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryOtherCloudConditions
   */
  public function getConditions()
  {
    return $this->conditions;
  }
  /**
   * @param GooglePrivacyDlpV2DataSourceType
   */
  public function setDataSourceType(GooglePrivacyDlpV2DataSourceType $dataSourceType)
  {
    $this->dataSourceType = $dataSourceType;
  }
  /**
   * @return GooglePrivacyDlpV2DataSourceType
   */
  public function getDataSourceType()
  {
    return $this->dataSourceType;
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
   * @param GooglePrivacyDlpV2DiscoveryOtherCloudFilter
   */
  public function setFilter(GooglePrivacyDlpV2DiscoveryOtherCloudFilter $filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryOtherCloudFilter
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryOtherCloudGenerationCadence
   */
  public function setGenerationCadence(GooglePrivacyDlpV2DiscoveryOtherCloudGenerationCadence $generationCadence)
  {
    $this->generationCadence = $generationCadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryOtherCloudGenerationCadence
   */
  public function getGenerationCadence()
  {
    return $this->generationCadence;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2OtherCloudDiscoveryTarget::class, 'Google_Service_DLP_GooglePrivacyDlpV2OtherCloudDiscoveryTarget');
