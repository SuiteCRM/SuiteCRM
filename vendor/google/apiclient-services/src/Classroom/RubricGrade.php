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

class RubricGrade extends \Google\Model
{
  /**
   * @var string
   */
  public $criterionId;
  /**
   * @var string
   */
  public $levelId;
  public $points;

  /**
   * @param string
   */
  public function setCriterionId($criterionId)
  {
    $this->criterionId = $criterionId;
  }
  /**
   * @return string
   */
  public function getCriterionId()
  {
    return $this->criterionId;
  }
  /**
   * @param string
   */
  public function setLevelId($levelId)
  {
    $this->levelId = $levelId;
  }
  /**
   * @return string
   */
  public function getLevelId()
  {
    return $this->levelId;
  }
  public function setPoints($points)
  {
    $this->points = $points;
  }
  public function getPoints()
  {
    return $this->points;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RubricGrade::class, 'Google_Service_Classroom_RubricGrade');
