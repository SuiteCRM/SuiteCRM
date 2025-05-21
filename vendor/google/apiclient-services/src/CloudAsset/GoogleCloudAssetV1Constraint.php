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

namespace Google\Service\CloudAsset;

class GoogleCloudAssetV1Constraint extends \Google\Model
{
  protected $booleanConstraintType = GoogleCloudAssetV1BooleanConstraint::class;
  protected $booleanConstraintDataType = '';
  /**
   * @var string
   */
  public $constraintDefault;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  protected $listConstraintType = GoogleCloudAssetV1ListConstraint::class;
  protected $listConstraintDataType = '';
  /**
   * @var string
   */
  public $name;

  /**
   * @param GoogleCloudAssetV1BooleanConstraint
   */
  public function setBooleanConstraint(GoogleCloudAssetV1BooleanConstraint $booleanConstraint)
  {
    $this->booleanConstraint = $booleanConstraint;
  }
  /**
   * @return GoogleCloudAssetV1BooleanConstraint
   */
  public function getBooleanConstraint()
  {
    return $this->booleanConstraint;
  }
  /**
   * @param string
   */
  public function setConstraintDefault($constraintDefault)
  {
    $this->constraintDefault = $constraintDefault;
  }
  /**
   * @return string
   */
  public function getConstraintDefault()
  {
    return $this->constraintDefault;
  }
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
   * @param GoogleCloudAssetV1ListConstraint
   */
  public function setListConstraint(GoogleCloudAssetV1ListConstraint $listConstraint)
  {
    $this->listConstraint = $listConstraint;
  }
  /**
   * @return GoogleCloudAssetV1ListConstraint
   */
  public function getListConstraint()
  {
    return $this->listConstraint;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAssetV1Constraint::class, 'Google_Service_CloudAsset_GoogleCloudAssetV1Constraint');
