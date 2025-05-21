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

namespace Google\Service\CloudRun;

class GoogleDevtoolsCloudbuildV1Results extends \Google\Collection
{
  protected $collection_key = 'pythonPackages';
  /**
   * @var string
   */
  public $artifactManifest;
  protected $artifactTimingType = GoogleDevtoolsCloudbuildV1TimeSpan::class;
  protected $artifactTimingDataType = '';
  /**
   * @var string[]
   */
  public $buildStepImages;
  /**
   * @var string[]
   */
  public $buildStepOutputs;
  protected $goModulesType = GoogleDevtoolsCloudbuildV1UploadedGoModule::class;
  protected $goModulesDataType = 'array';
  protected $imagesType = GoogleDevtoolsCloudbuildV1BuiltImage::class;
  protected $imagesDataType = 'array';
  protected $mavenArtifactsType = GoogleDevtoolsCloudbuildV1UploadedMavenArtifact::class;
  protected $mavenArtifactsDataType = 'array';
  protected $npmPackagesType = GoogleDevtoolsCloudbuildV1UploadedNpmPackage::class;
  protected $npmPackagesDataType = 'array';
  /**
   * @var string
   */
  public $numArtifacts;
  protected $pythonPackagesType = GoogleDevtoolsCloudbuildV1UploadedPythonPackage::class;
  protected $pythonPackagesDataType = 'array';

  /**
   * @param string
   */
  public function setArtifactManifest($artifactManifest)
  {
    $this->artifactManifest = $artifactManifest;
  }
  /**
   * @return string
   */
  public function getArtifactManifest()
  {
    return $this->artifactManifest;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1TimeSpan
   */
  public function setArtifactTiming(GoogleDevtoolsCloudbuildV1TimeSpan $artifactTiming)
  {
    $this->artifactTiming = $artifactTiming;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1TimeSpan
   */
  public function getArtifactTiming()
  {
    return $this->artifactTiming;
  }
  /**
   * @param string[]
   */
  public function setBuildStepImages($buildStepImages)
  {
    $this->buildStepImages = $buildStepImages;
  }
  /**
   * @return string[]
   */
  public function getBuildStepImages()
  {
    return $this->buildStepImages;
  }
  /**
   * @param string[]
   */
  public function setBuildStepOutputs($buildStepOutputs)
  {
    $this->buildStepOutputs = $buildStepOutputs;
  }
  /**
   * @return string[]
   */
  public function getBuildStepOutputs()
  {
    return $this->buildStepOutputs;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1UploadedGoModule[]
   */
  public function setGoModules($goModules)
  {
    $this->goModules = $goModules;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1UploadedGoModule[]
   */
  public function getGoModules()
  {
    return $this->goModules;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1BuiltImage[]
   */
  public function setImages($images)
  {
    $this->images = $images;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1BuiltImage[]
   */
  public function getImages()
  {
    return $this->images;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1UploadedMavenArtifact[]
   */
  public function setMavenArtifacts($mavenArtifacts)
  {
    $this->mavenArtifacts = $mavenArtifacts;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1UploadedMavenArtifact[]
   */
  public function getMavenArtifacts()
  {
    return $this->mavenArtifacts;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1UploadedNpmPackage[]
   */
  public function setNpmPackages($npmPackages)
  {
    $this->npmPackages = $npmPackages;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1UploadedNpmPackage[]
   */
  public function getNpmPackages()
  {
    return $this->npmPackages;
  }
  /**
   * @param string
   */
  public function setNumArtifacts($numArtifacts)
  {
    $this->numArtifacts = $numArtifacts;
  }
  /**
   * @return string
   */
  public function getNumArtifacts()
  {
    return $this->numArtifacts;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1UploadedPythonPackage[]
   */
  public function setPythonPackages($pythonPackages)
  {
    $this->pythonPackages = $pythonPackages;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1UploadedPythonPackage[]
   */
  public function getPythonPackages()
  {
    return $this->pythonPackages;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsCloudbuildV1Results::class, 'Google_Service_CloudRun_GoogleDevtoolsCloudbuildV1Results');
