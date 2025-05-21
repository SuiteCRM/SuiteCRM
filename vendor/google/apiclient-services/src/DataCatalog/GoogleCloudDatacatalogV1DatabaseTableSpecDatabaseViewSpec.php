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

namespace Google\Service\DataCatalog;

class GoogleCloudDatacatalogV1DatabaseTableSpecDatabaseViewSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $baseTable;
  /**
   * @var string
   */
  public $sqlQuery;
  /**
   * @var string
   */
  public $viewType;

  /**
   * @param string
   */
  public function setBaseTable($baseTable)
  {
    $this->baseTable = $baseTable;
  }
  /**
   * @return string
   */
  public function getBaseTable()
  {
    return $this->baseTable;
  }
  /**
   * @param string
   */
  public function setSqlQuery($sqlQuery)
  {
    $this->sqlQuery = $sqlQuery;
  }
  /**
   * @return string
   */
  public function getSqlQuery()
  {
    return $this->sqlQuery;
  }
  /**
   * @param string
   */
  public function setViewType($viewType)
  {
    $this->viewType = $viewType;
  }
  /**
   * @return string
   */
  public function getViewType()
  {
    return $this->viewType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDatacatalogV1DatabaseTableSpecDatabaseViewSpec::class, 'Google_Service_DataCatalog_GoogleCloudDatacatalogV1DatabaseTableSpecDatabaseViewSpec');
