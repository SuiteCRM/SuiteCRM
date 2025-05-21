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

class MappingRuleFilter extends \Google\Collection
{
  protected $collection_key = 'entities';
  /**
   * @var string[]
   */
  public $entities;
  /**
   * @var string
   */
  public $entityNameContains;
  /**
   * @var string
   */
  public $entityNamePrefix;
  /**
   * @var string
   */
  public $entityNameSuffix;
  /**
   * @var string
   */
  public $parentEntity;

  /**
   * @param string[]
   */
  public function setEntities($entities)
  {
    $this->entities = $entities;
  }
  /**
   * @return string[]
   */
  public function getEntities()
  {
    return $this->entities;
  }
  /**
   * @param string
   */
  public function setEntityNameContains($entityNameContains)
  {
    $this->entityNameContains = $entityNameContains;
  }
  /**
   * @return string
   */
  public function getEntityNameContains()
  {
    return $this->entityNameContains;
  }
  /**
   * @param string
   */
  public function setEntityNamePrefix($entityNamePrefix)
  {
    $this->entityNamePrefix = $entityNamePrefix;
  }
  /**
   * @return string
   */
  public function getEntityNamePrefix()
  {
    return $this->entityNamePrefix;
  }
  /**
   * @param string
   */
  public function setEntityNameSuffix($entityNameSuffix)
  {
    $this->entityNameSuffix = $entityNameSuffix;
  }
  /**
   * @return string
   */
  public function getEntityNameSuffix()
  {
    return $this->entityNameSuffix;
  }
  /**
   * @param string
   */
  public function setParentEntity($parentEntity)
  {
    $this->parentEntity = $parentEntity;
  }
  /**
   * @return string
   */
  public function getParentEntity()
  {
    return $this->parentEntity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MappingRuleFilter::class, 'Google_Service_DatabaseMigrationService_MappingRuleFilter');
