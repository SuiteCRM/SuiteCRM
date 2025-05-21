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

namespace Google\Service\OracleDatabase;

class AutonomousDatabaseConnectionUrls extends \Google\Model
{
  /**
   * @var string
   */
  public $apexUri;
  /**
   * @var string
   */
  public $databaseTransformsUri;
  /**
   * @var string
   */
  public $graphStudioUri;
  /**
   * @var string
   */
  public $machineLearningNotebookUri;
  /**
   * @var string
   */
  public $machineLearningUserManagementUri;
  /**
   * @var string
   */
  public $mongoDbUri;
  /**
   * @var string
   */
  public $ordsUri;
  /**
   * @var string
   */
  public $sqlDevWebUri;

  /**
   * @param string
   */
  public function setApexUri($apexUri)
  {
    $this->apexUri = $apexUri;
  }
  /**
   * @return string
   */
  public function getApexUri()
  {
    return $this->apexUri;
  }
  /**
   * @param string
   */
  public function setDatabaseTransformsUri($databaseTransformsUri)
  {
    $this->databaseTransformsUri = $databaseTransformsUri;
  }
  /**
   * @return string
   */
  public function getDatabaseTransformsUri()
  {
    return $this->databaseTransformsUri;
  }
  /**
   * @param string
   */
  public function setGraphStudioUri($graphStudioUri)
  {
    $this->graphStudioUri = $graphStudioUri;
  }
  /**
   * @return string
   */
  public function getGraphStudioUri()
  {
    return $this->graphStudioUri;
  }
  /**
   * @param string
   */
  public function setMachineLearningNotebookUri($machineLearningNotebookUri)
  {
    $this->machineLearningNotebookUri = $machineLearningNotebookUri;
  }
  /**
   * @return string
   */
  public function getMachineLearningNotebookUri()
  {
    return $this->machineLearningNotebookUri;
  }
  /**
   * @param string
   */
  public function setMachineLearningUserManagementUri($machineLearningUserManagementUri)
  {
    $this->machineLearningUserManagementUri = $machineLearningUserManagementUri;
  }
  /**
   * @return string
   */
  public function getMachineLearningUserManagementUri()
  {
    return $this->machineLearningUserManagementUri;
  }
  /**
   * @param string
   */
  public function setMongoDbUri($mongoDbUri)
  {
    $this->mongoDbUri = $mongoDbUri;
  }
  /**
   * @return string
   */
  public function getMongoDbUri()
  {
    return $this->mongoDbUri;
  }
  /**
   * @param string
   */
  public function setOrdsUri($ordsUri)
  {
    $this->ordsUri = $ordsUri;
  }
  /**
   * @return string
   */
  public function getOrdsUri()
  {
    return $this->ordsUri;
  }
  /**
   * @param string
   */
  public function setSqlDevWebUri($sqlDevWebUri)
  {
    $this->sqlDevWebUri = $sqlDevWebUri;
  }
  /**
   * @return string
   */
  public function getSqlDevWebUri()
  {
    return $this->sqlDevWebUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDatabaseConnectionUrls::class, 'Google_Service_OracleDatabase_AutonomousDatabaseConnectionUrls');
