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

namespace Google\Service\MigrationCenterAPI;

class ErrorFrame extends \Google\Collection
{
  protected $collection_key = 'violations';
  /**
   * @var string
   */
  public $ingestionTime;
  /**
   * @var string
   */
  public $name;
  protected $originalFrameType = AssetFrame::class;
  protected $originalFrameDataType = '';
  protected $violationsType = FrameViolationEntry::class;
  protected $violationsDataType = 'array';

  /**
   * @param string
   */
  public function setIngestionTime($ingestionTime)
  {
    $this->ingestionTime = $ingestionTime;
  }
  /**
   * @return string
   */
  public function getIngestionTime()
  {
    return $this->ingestionTime;
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
   * @param AssetFrame
   */
  public function setOriginalFrame(AssetFrame $originalFrame)
  {
    $this->originalFrame = $originalFrame;
  }
  /**
   * @return AssetFrame
   */
  public function getOriginalFrame()
  {
    return $this->originalFrame;
  }
  /**
   * @param FrameViolationEntry[]
   */
  public function setViolations($violations)
  {
    $this->violations = $violations;
  }
  /**
   * @return FrameViolationEntry[]
   */
  public function getViolations()
  {
    return $this->violations;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ErrorFrame::class, 'Google_Service_MigrationCenterAPI_ErrorFrame');
