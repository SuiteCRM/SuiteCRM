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

namespace Google\Service\SecurityCommandCenter;

class KernelRootkit extends \Google\Model
{
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $unexpectedCodeModification;
  /**
   * @var bool
   */
  public $unexpectedFtraceHandler;
  /**
   * @var bool
   */
  public $unexpectedInterruptHandler;
  /**
   * @var bool
   */
  public $unexpectedKernelCodePages;
  /**
   * @var bool
   */
  public $unexpectedKprobeHandler;
  /**
   * @var bool
   */
  public $unexpectedProcessesInRunqueue;
  /**
   * @var bool
   */
  public $unexpectedReadOnlyDataModification;
  /**
   * @var bool
   */
  public $unexpectedSystemCallHandler;

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
   * @param bool
   */
  public function setUnexpectedCodeModification($unexpectedCodeModification)
  {
    $this->unexpectedCodeModification = $unexpectedCodeModification;
  }
  /**
   * @return bool
   */
  public function getUnexpectedCodeModification()
  {
    return $this->unexpectedCodeModification;
  }
  /**
   * @param bool
   */
  public function setUnexpectedFtraceHandler($unexpectedFtraceHandler)
  {
    $this->unexpectedFtraceHandler = $unexpectedFtraceHandler;
  }
  /**
   * @return bool
   */
  public function getUnexpectedFtraceHandler()
  {
    return $this->unexpectedFtraceHandler;
  }
  /**
   * @param bool
   */
  public function setUnexpectedInterruptHandler($unexpectedInterruptHandler)
  {
    $this->unexpectedInterruptHandler = $unexpectedInterruptHandler;
  }
  /**
   * @return bool
   */
  public function getUnexpectedInterruptHandler()
  {
    return $this->unexpectedInterruptHandler;
  }
  /**
   * @param bool
   */
  public function setUnexpectedKernelCodePages($unexpectedKernelCodePages)
  {
    $this->unexpectedKernelCodePages = $unexpectedKernelCodePages;
  }
  /**
   * @return bool
   */
  public function getUnexpectedKernelCodePages()
  {
    return $this->unexpectedKernelCodePages;
  }
  /**
   * @param bool
   */
  public function setUnexpectedKprobeHandler($unexpectedKprobeHandler)
  {
    $this->unexpectedKprobeHandler = $unexpectedKprobeHandler;
  }
  /**
   * @return bool
   */
  public function getUnexpectedKprobeHandler()
  {
    return $this->unexpectedKprobeHandler;
  }
  /**
   * @param bool
   */
  public function setUnexpectedProcessesInRunqueue($unexpectedProcessesInRunqueue)
  {
    $this->unexpectedProcessesInRunqueue = $unexpectedProcessesInRunqueue;
  }
  /**
   * @return bool
   */
  public function getUnexpectedProcessesInRunqueue()
  {
    return $this->unexpectedProcessesInRunqueue;
  }
  /**
   * @param bool
   */
  public function setUnexpectedReadOnlyDataModification($unexpectedReadOnlyDataModification)
  {
    $this->unexpectedReadOnlyDataModification = $unexpectedReadOnlyDataModification;
  }
  /**
   * @return bool
   */
  public function getUnexpectedReadOnlyDataModification()
  {
    return $this->unexpectedReadOnlyDataModification;
  }
  /**
   * @param bool
   */
  public function setUnexpectedSystemCallHandler($unexpectedSystemCallHandler)
  {
    $this->unexpectedSystemCallHandler = $unexpectedSystemCallHandler;
  }
  /**
   * @return bool
   */
  public function getUnexpectedSystemCallHandler()
  {
    return $this->unexpectedSystemCallHandler;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(KernelRootkit::class, 'Google_Service_SecurityCommandCenter_KernelRootkit');
