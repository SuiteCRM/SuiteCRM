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

class GooglePrivacyDlpV2DiscoveryBigQueryConditions extends \Google\Model
{
  /**
   * @var string
   */
  public $createdAfter;
  protected $orConditionsType = GooglePrivacyDlpV2OrConditions::class;
  protected $orConditionsDataType = '';
  /**
   * @var string
   */
  public $typeCollection;
  protected $typesType = GooglePrivacyDlpV2BigQueryTableTypes::class;
  protected $typesDataType = '';

  /**
   * @param string
   */
  public function setCreatedAfter($createdAfter)
  {
    $this->createdAfter = $createdAfter;
  }
  /**
   * @return string
   */
  public function getCreatedAfter()
  {
    return $this->createdAfter;
  }
  /**
   * @param GooglePrivacyDlpV2OrConditions
   */
  public function setOrConditions(GooglePrivacyDlpV2OrConditions $orConditions)
  {
    $this->orConditions = $orConditions;
  }
  /**
   * @return GooglePrivacyDlpV2OrConditions
   */
  public function getOrConditions()
  {
    return $this->orConditions;
  }
  /**
   * @param string
   */
  public function setTypeCollection($typeCollection)
  {
    $this->typeCollection = $typeCollection;
  }
  /**
   * @return string
   */
  public function getTypeCollection()
  {
    return $this->typeCollection;
  }
  /**
   * @param GooglePrivacyDlpV2BigQueryTableTypes
   */
  public function setTypes(GooglePrivacyDlpV2BigQueryTableTypes $types)
  {
    $this->types = $types;
  }
  /**
   * @return GooglePrivacyDlpV2BigQueryTableTypes
   */
  public function getTypes()
  {
    return $this->types;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryBigQueryConditions::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryBigQueryConditions');
