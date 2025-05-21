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

namespace Google\Service\Classroom;

class Rubric extends \Google\Collection
{
  protected $collection_key = 'criteria';
  /**
   * @var string
   */
  public $courseId;
  /**
   * @var string
   */
  public $courseWorkId;
  /**
   * @var string
   */
  public $creationTime;
  protected $criteriaType = Criterion::class;
  protected $criteriaDataType = 'array';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $sourceSpreadsheetId;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCourseId($courseId)
  {
    $this->courseId = $courseId;
  }
  /**
   * @return string
   */
  public function getCourseId()
  {
    return $this->courseId;
  }
  /**
   * @param string
   */
  public function setCourseWorkId($courseWorkId)
  {
    $this->courseWorkId = $courseWorkId;
  }
  /**
   * @return string
   */
  public function getCourseWorkId()
  {
    return $this->courseWorkId;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param Criterion[]
   */
  public function setCriteria($criteria)
  {
    $this->criteria = $criteria;
  }
  /**
   * @return Criterion[]
   */
  public function getCriteria()
  {
    return $this->criteria;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setSourceSpreadsheetId($sourceSpreadsheetId)
  {
    $this->sourceSpreadsheetId = $sourceSpreadsheetId;
  }
  /**
   * @return string
   */
  public function getSourceSpreadsheetId()
  {
    return $this->sourceSpreadsheetId;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Rubric::class, 'Google_Service_Classroom_Rubric');
