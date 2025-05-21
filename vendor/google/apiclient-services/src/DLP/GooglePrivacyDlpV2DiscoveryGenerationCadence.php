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

class GooglePrivacyDlpV2DiscoveryGenerationCadence extends \Google\Model
{
  protected $inspectTemplateModifiedCadenceType = GooglePrivacyDlpV2DiscoveryInspectTemplateModifiedCadence::class;
  protected $inspectTemplateModifiedCadenceDataType = '';
  /**
   * @var string
   */
  public $refreshFrequency;
  protected $schemaModifiedCadenceType = GooglePrivacyDlpV2DiscoverySchemaModifiedCadence::class;
  protected $schemaModifiedCadenceDataType = '';
  protected $tableModifiedCadenceType = GooglePrivacyDlpV2DiscoveryTableModifiedCadence::class;
  protected $tableModifiedCadenceDataType = '';

  /**
   * @param GooglePrivacyDlpV2DiscoveryInspectTemplateModifiedCadence
   */
  public function setInspectTemplateModifiedCadence(GooglePrivacyDlpV2DiscoveryInspectTemplateModifiedCadence $inspectTemplateModifiedCadence)
  {
    $this->inspectTemplateModifiedCadence = $inspectTemplateModifiedCadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryInspectTemplateModifiedCadence
   */
  public function getInspectTemplateModifiedCadence()
  {
    return $this->inspectTemplateModifiedCadence;
  }
  /**
   * @param string
   */
  public function setRefreshFrequency($refreshFrequency)
  {
    $this->refreshFrequency = $refreshFrequency;
  }
  /**
   * @return string
   */
  public function getRefreshFrequency()
  {
    return $this->refreshFrequency;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoverySchemaModifiedCadence
   */
  public function setSchemaModifiedCadence(GooglePrivacyDlpV2DiscoverySchemaModifiedCadence $schemaModifiedCadence)
  {
    $this->schemaModifiedCadence = $schemaModifiedCadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoverySchemaModifiedCadence
   */
  public function getSchemaModifiedCadence()
  {
    return $this->schemaModifiedCadence;
  }
  /**
   * @param GooglePrivacyDlpV2DiscoveryTableModifiedCadence
   */
  public function setTableModifiedCadence(GooglePrivacyDlpV2DiscoveryTableModifiedCadence $tableModifiedCadence)
  {
    $this->tableModifiedCadence = $tableModifiedCadence;
  }
  /**
   * @return GooglePrivacyDlpV2DiscoveryTableModifiedCadence
   */
  public function getTableModifiedCadence()
  {
    return $this->tableModifiedCadence;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryGenerationCadence::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryGenerationCadence');
