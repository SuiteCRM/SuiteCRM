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

class EntityDdl extends \Google\Collection
{
  protected $collection_key = 'issueId';
  /**
   * @var string
   */
  public $ddl;
  /**
   * @var string
   */
  public $ddlKind;
  /**
   * @var string
   */
  public $ddlType;
  /**
   * @var string
   */
  public $editedDdlKind;
  /**
   * @var string
   */
  public $entity;
  /**
   * @var string
   */
  public $entityType;
  /**
   * @var string[]
   */
  public $issueId;

  /**
   * @param string
   */
  public function setDdl($ddl)
  {
    $this->ddl = $ddl;
  }
  /**
   * @return string
   */
  public function getDdl()
  {
    return $this->ddl;
  }
  /**
   * @param string
   */
  public function setDdlKind($ddlKind)
  {
    $this->ddlKind = $ddlKind;
  }
  /**
   * @return string
   */
  public function getDdlKind()
  {
    return $this->ddlKind;
  }
  /**
   * @param string
   */
  public function setDdlType($ddlType)
  {
    $this->ddlType = $ddlType;
  }
  /**
   * @return string
   */
  public function getDdlType()
  {
    return $this->ddlType;
  }
  /**
   * @param string
   */
  public function setEditedDdlKind($editedDdlKind)
  {
    $this->editedDdlKind = $editedDdlKind;
  }
  /**
   * @return string
   */
  public function getEditedDdlKind()
  {
    return $this->editedDdlKind;
  }
  /**
   * @param string
   */
  public function setEntity($entity)
  {
    $this->entity = $entity;
  }
  /**
   * @return string
   */
  public function getEntity()
  {
    return $this->entity;
  }
  /**
   * @param string
   */
  public function setEntityType($entityType)
  {
    $this->entityType = $entityType;
  }
  /**
   * @return string
   */
  public function getEntityType()
  {
    return $this->entityType;
  }
  /**
   * @param string[]
   */
  public function setIssueId($issueId)
  {
    $this->issueId = $issueId;
  }
  /**
   * @return string[]
   */
  public function getIssueId()
  {
    return $this->issueId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EntityDdl::class, 'Google_Service_DatabaseMigrationService_EntityDdl');
