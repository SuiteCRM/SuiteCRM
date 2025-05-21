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

class GooglePrivacyDlpV2BigQueryRegex extends \Google\Model
{
  /**
   * @var string
   */
  public $datasetIdRegex;
  /**
   * @var string
   */
  public $projectIdRegex;
  /**
   * @var string
   */
  public $tableIdRegex;

  /**
   * @param string
   */
  public function setDatasetIdRegex($datasetIdRegex)
  {
    $this->datasetIdRegex = $datasetIdRegex;
  }
  /**
   * @return string
   */
  public function getDatasetIdRegex()
  {
    return $this->datasetIdRegex;
  }
  /**
   * @param string
   */
  public function setProjectIdRegex($projectIdRegex)
  {
    $this->projectIdRegex = $projectIdRegex;
  }
  /**
   * @return string
   */
  public function getProjectIdRegex()
  {
    return $this->projectIdRegex;
  }
  /**
   * @param string
   */
  public function setTableIdRegex($tableIdRegex)
  {
    $this->tableIdRegex = $tableIdRegex;
  }
  /**
   * @return string
   */
  public function getTableIdRegex()
  {
    return $this->tableIdRegex;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2BigQueryRegex::class, 'Google_Service_DLP_GooglePrivacyDlpV2BigQueryRegex');
