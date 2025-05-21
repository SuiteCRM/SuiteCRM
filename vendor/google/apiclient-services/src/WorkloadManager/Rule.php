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

namespace Google\Service\WorkloadManager;

class Rule extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $errorMessage;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $primaryCategory;
  /**
   * @var string
   */
  public $remediation;
  /**
   * @var string
   */
  public $revisionId;
  /**
   * @var string
   */
  public $secondaryCategory;
  /**
   * @var string
   */
  public $severity;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $uri;

  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setErrorMessage($errorMessage)
  {
    $this->errorMessage = $errorMessage;
  }
  /**
   * @return string
   */
  public function getErrorMessage()
  {
    return $this->errorMessage;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setPrimaryCategory($primaryCategory)
  {
    $this->primaryCategory = $primaryCategory;
  }
  /**
   * @return string
   */
  public function getPrimaryCategory()
  {
    return $this->primaryCategory;
  }
  /**
   * @param string
   */
  public function setRemediation($remediation)
  {
    $this->remediation = $remediation;
  }
  /**
   * @return string
   */
  public function getRemediation()
  {
    return $this->remediation;
  }
  /**
   * @param string
   */
  public function setRevisionId($revisionId)
  {
    $this->revisionId = $revisionId;
  }
  /**
   * @return string
   */
  public function getRevisionId()
  {
    return $this->revisionId;
  }
  /**
   * @param string
   */
  public function setSecondaryCategory($secondaryCategory)
  {
    $this->secondaryCategory = $secondaryCategory;
  }
  /**
   * @return string
   */
  public function getSecondaryCategory()
  {
    return $this->secondaryCategory;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Rule::class, 'Google_Service_WorkloadManager_Rule');
