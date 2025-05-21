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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $documentId;
  /**
   * @var bool
   */
  public $isPrivate;
  /**
   * @var string
   */
  public $lastCommentTimeMs;
  /**
   * @var string
   */
  public $lastEditTimeMs;
  /**
   * @var string
   */
  public $lastModificationTimeMillis;
  /**
   * @var string
   */
  public $lastUpdatedTimeMs;
  /**
   * @var string
   */
  public $lastViewTimeMs;
  protected $ownerType = EnterpriseTopazSidekickCommonPerson::class;
  protected $ownerDataType = '';
  /**
   * @var string
   */
  public $scope;

  /**
   * @param string
   */
  public function setDocumentId($documentId)
  {
    $this->documentId = $documentId;
  }
  /**
   * @return string
   */
  public function getDocumentId()
  {
    return $this->documentId;
  }
  /**
   * @param bool
   */
  public function setIsPrivate($isPrivate)
  {
    $this->isPrivate = $isPrivate;
  }
  /**
   * @return bool
   */
  public function getIsPrivate()
  {
    return $this->isPrivate;
  }
  /**
   * @param string
   */
  public function setLastCommentTimeMs($lastCommentTimeMs)
  {
    $this->lastCommentTimeMs = $lastCommentTimeMs;
  }
  /**
   * @return string
   */
  public function getLastCommentTimeMs()
  {
    return $this->lastCommentTimeMs;
  }
  /**
   * @param string
   */
  public function setLastEditTimeMs($lastEditTimeMs)
  {
    $this->lastEditTimeMs = $lastEditTimeMs;
  }
  /**
   * @return string
   */
  public function getLastEditTimeMs()
  {
    return $this->lastEditTimeMs;
  }
  /**
   * @param string
   */
  public function setLastModificationTimeMillis($lastModificationTimeMillis)
  {
    $this->lastModificationTimeMillis = $lastModificationTimeMillis;
  }
  /**
   * @return string
   */
  public function getLastModificationTimeMillis()
  {
    return $this->lastModificationTimeMillis;
  }
  /**
   * @param string
   */
  public function setLastUpdatedTimeMs($lastUpdatedTimeMs)
  {
    $this->lastUpdatedTimeMs = $lastUpdatedTimeMs;
  }
  /**
   * @return string
   */
  public function getLastUpdatedTimeMs()
  {
    return $this->lastUpdatedTimeMs;
  }
  /**
   * @param string
   */
  public function setLastViewTimeMs($lastViewTimeMs)
  {
    $this->lastViewTimeMs = $lastViewTimeMs;
  }
  /**
   * @return string
   */
  public function getLastViewTimeMs()
  {
    return $this->lastViewTimeMs;
  }
  /**
   * @param EnterpriseTopazSidekickCommonPerson
   */
  public function setOwner(EnterpriseTopazSidekickCommonPerson $owner)
  {
    $this->owner = $owner;
  }
  /**
   * @return EnterpriseTopazSidekickCommonPerson
   */
  public function getOwner()
  {
    return $this->owner;
  }
  /**
   * @param string
   */
  public function setScope($scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return string
   */
  public function getScope()
  {
    return $this->scope;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickCommonDocumentDriveDocumentMetadata');
