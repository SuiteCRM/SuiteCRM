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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1MetadataJobImportJobResult extends \Google\Model
{
  /**
   * @var string
   */
  public $createdEntries;
  /**
   * @var string
   */
  public $deletedEntries;
  /**
   * @var string
   */
  public $recreatedEntries;
  /**
   * @var string
   */
  public $unchangedEntries;
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $updatedEntries;

  /**
   * @param string
   */
  public function setCreatedEntries($createdEntries)
  {
    $this->createdEntries = $createdEntries;
  }
  /**
   * @return string
   */
  public function getCreatedEntries()
  {
    return $this->createdEntries;
  }
  /**
   * @param string
   */
  public function setDeletedEntries($deletedEntries)
  {
    $this->deletedEntries = $deletedEntries;
  }
  /**
   * @return string
   */
  public function getDeletedEntries()
  {
    return $this->deletedEntries;
  }
  /**
   * @param string
   */
  public function setRecreatedEntries($recreatedEntries)
  {
    $this->recreatedEntries = $recreatedEntries;
  }
  /**
   * @return string
   */
  public function getRecreatedEntries()
  {
    return $this->recreatedEntries;
  }
  /**
   * @param string
   */
  public function setUnchangedEntries($unchangedEntries)
  {
    $this->unchangedEntries = $unchangedEntries;
  }
  /**
   * @return string
   */
  public function getUnchangedEntries()
  {
    return $this->unchangedEntries;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setUpdatedEntries($updatedEntries)
  {
    $this->updatedEntries = $updatedEntries;
  }
  /**
   * @return string
   */
  public function getUpdatedEntries()
  {
    return $this->updatedEntries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1MetadataJobImportJobResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1MetadataJobImportJobResult');
