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

class AddOnContext extends \Google\Model
{
  /**
   * @var string
   */
  public $courseId;
  /**
   * @var string
   */
  public $itemId;
  /**
   * @var string
   */
  public $postId;
  protected $studentContextType = StudentContext::class;
  protected $studentContextDataType = '';
  /**
   * @var bool
   */
  public $supportsStudentWork;
  protected $teacherContextType = TeacherContext::class;
  protected $teacherContextDataType = '';

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
   * @param StudentContext
   */
  public function setStudentContext(StudentContext $studentContext)
  {
    $this->studentContext = $studentContext;
  }
  /**
   * @return StudentContext
   */
  public function getStudentContext()
  {
    return $this->studentContext;
  }
  /**
   * @param bool
   */
  public function setSupportsStudentWork($supportsStudentWork)
  {
    $this->supportsStudentWork = $supportsStudentWork;
  }
  /**
   * @return bool
   */
  public function getSupportsStudentWork()
  {
    return $this->supportsStudentWork;
  }
  /**
   * @param TeacherContext
   */
  public function setTeacherContext(TeacherContext $teacherContext)
  {
    $this->teacherContext = $teacherContext;
  }
  /**
   * @return TeacherContext
   */
  public function getTeacherContext()
  {
    return $this->teacherContext;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AddOnContext::class, 'Google_Service_Classroom_AddOnContext');
