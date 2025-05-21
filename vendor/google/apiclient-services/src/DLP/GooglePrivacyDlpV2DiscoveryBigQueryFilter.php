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

class GooglePrivacyDlpV2DiscoveryBigQueryFilter extends \Google\Model
{
  protected $otherTablesType = GooglePrivacyDlpV2AllOtherBigQueryTables::class;
  protected $otherTablesDataType = '';
  protected $tableReferenceType = GooglePrivacyDlpV2TableReference::class;
  protected $tableReferenceDataType = '';
  protected $tablesType = GooglePrivacyDlpV2BigQueryTableCollection::class;
  protected $tablesDataType = '';

  /**
   * @param GooglePrivacyDlpV2AllOtherBigQueryTables
   */
  public function setOtherTables(GooglePrivacyDlpV2AllOtherBigQueryTables $otherTables)
  {
    $this->otherTables = $otherTables;
  }
  /**
   * @return GooglePrivacyDlpV2AllOtherBigQueryTables
   */
  public function getOtherTables()
  {
    return $this->otherTables;
  }
  /**
   * @param GooglePrivacyDlpV2TableReference
   */
  public function setTableReference(GooglePrivacyDlpV2TableReference $tableReference)
  {
    $this->tableReference = $tableReference;
  }
  /**
   * @return GooglePrivacyDlpV2TableReference
   */
  public function getTableReference()
  {
    return $this->tableReference;
  }
  /**
   * @param GooglePrivacyDlpV2BigQueryTableCollection
   */
  public function setTables(GooglePrivacyDlpV2BigQueryTableCollection $tables)
  {
    $this->tables = $tables;
  }
  /**
   * @return GooglePrivacyDlpV2BigQueryTableCollection
   */
  public function getTables()
  {
    return $this->tables;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryBigQueryFilter::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryBigQueryFilter');
