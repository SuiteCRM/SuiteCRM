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

class AddOnAttachment extends \Google\Collection
{
  protected $collection_key = 'copyHistory';
  protected $copyHistoryType = CopyHistory::class;
  protected $copyHistoryDataType = 'array';
  /**
   * @var string
   */
  public $courseId;
  protected $dueDateType = Date::class;
  protected $dueDateDataType = '';
  protected $dueTimeType = TimeOfDay::class;
  protected $dueTimeDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $itemId;
  public $maxPoints;
  /**
   * @var string
   */
  public $postId;
  protected $studentViewUriType = EmbedUri::class;
  protected $studentViewUriDataType = '';
  protected $studentWorkReviewUriType = EmbedUri::class;
  protected $studentWorkReviewUriDataType = '';
  protected $teacherViewUriType = EmbedUri::class;
  protected $teacherViewUriDataType = '';
  /**
   * @var string
   */
  public $title;

  /**
   * @param CopyHistory[]
   */
  public function setCopyHistory($copyHistory)
  {
    $this->copyHistory = $copyHistory;
  }
  /**
   * @return CopyHistory[]
   */
  public function getCopyHistory()
  {
    return $this->copyHistory;
  }
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
   * @param Date
   */
  public function setDueDate(Date $dueDate)
  {
    $this->dueDate = $dueDate;
  }
  /**
   * @return Date
   */
  public function getDueDate()
  {
    return $this->dueDate;
  }
  /**
   * @param TimeOfDay
   */
  public function setDueTime(TimeOfDay $dueTime)
  {
    $this->dueTime = $dueTime;
  }
  /**
   * @return TimeOfDay
   */
  public function getDueTime()
  {
    return $this->dueTime;
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
  public function setItemId($itemId)
  {
    $this->itemId = $itemId;
  }
  /**
   * @return string
   */
  public function getItemId()
  {
    return $this->itemId;
  }
  public function setMaxPoints($maxPoints)
  {
    $this->maxPoints = $maxPoints;
  }
  public function getMaxPoints()
  {
    return $this->maxPoints;
  }
  /**
   * @param string
   */
  public function setPostId($postId)
  {
    $this->postId = $postId;
  }
  /**
   * @return string
   */
  public function getPostId()
  {
    return $this->postId;
  }
  /**
   * @param EmbedUri
   */
  public function setStudentViewUri(EmbedUri $studentViewUri)
  {
    $this->studentViewUri = $studentViewUri;
  }
  /**
   * @return EmbedUri
   */
  public function getStudentViewUri()
  {
    return $this->studentViewUri;
  }
  /**
   * @param EmbedUri
   */
  public function setStudentWorkReviewUri(EmbedUri $studentWorkReviewUri)
  {
    $this->studentWorkReviewUri = $studentWorkReviewUri;
  }
  /**
   * @return EmbedUri
   */
  public function getStudentWorkReviewUri()
  {
    return $this->studentWorkReviewUri;
  }
  /**
   * @param EmbedUri
   */
  public function setTeacherViewUri(EmbedUri $teacherViewUri)
  {
    $this->teacherViewUri = $teacherViewUri;
  }
  /**
   * @return EmbedUri
   */
  public function getTeacherViewUri()
  {
    return $this->teacherViewUri;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AddOnAttachment::class, 'Google_Service_Classroom_AddOnAttachment');
