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

namespace Google\Service\DatabaseMigrationService;

class EntityMapping extends \Google\Collection
{
  protected $collection_key = 'mappingLog';
  /**
   * @var string
   */
  public $draftEntity;
  /**
   * @var string
   */
  public $draftType;
  protected $mappingLogType = EntityMappingLogEntry::class;
  protected $mappingLogDataType = 'array';
  /**
   * @var string
   */
  public $sourceEntity;
  /**
   * @var string
   */
  public $sourceType;

  /**
   * @param string
   */
  public function setDraftEntity($draftEntity)
  {
    $this->draftEntity = $draftEntity;
  }
  /**
   * @return string
   */
  public function getDraftEntity()
  {
    return $this->draftEntity;
  }
  /**
   * @param string
   */
  public function setDraftType($draftType)
  {
    $this->draftType = $draftType;
  }
  /**
   * @return string
   */
  public function getDraftType()
  {
    return $this->draftType;
  }
  /**
   * @param EntityMappingLogEntry[]
   */
  public function setMappingLog($mappingLog)
  {
    $this->mappingLog = $mappingLog;
  }
  /**
   * @return EntityMappingLogEntry[]
   */
  public function getMappingLog()
  {
    return $this->mappingLog;
  }
  /**
   * @param string
   */
  public function setSourceEntity($sourceEntity)
  {
    $this->sourceEntity = $sourceEntity;
  }
  /**
   * @return string
   */
  public function getSourceEntity()
  {
    return $this->sourceEntity;
  }
  /**
   * @param string
   */
  public function setSourceType($sourceType)
  {
    $this->sourceType = $sourceType;
  }
  /**
   * @return string
   */
  public function getSourceType()
  {
    return $this->sourceType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EntityMapping::class, 'Google_Service_DatabaseMigrationService_EntityMapping');
