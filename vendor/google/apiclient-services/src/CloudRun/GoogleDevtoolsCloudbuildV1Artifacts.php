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

class GoogleDevtoolsCloudbuildV1Artifacts extends \Google\Collection
{
  protected $collection_key = 'pythonPackages';
  protected $goModulesType = GoogleDevtoolsCloudbuildV1GoModule::class;
  protected $goModulesDataType = 'array';
  /**
   * @var string[]
   */
  public $images;
  protected $mavenArtifactsType = GoogleDevtoolsCloudbuildV1MavenArtifact::class;
  protected $mavenArtifactsDataType = 'array';
  protected $npmPackagesType = GoogleDevtoolsCloudbuildV1NpmPackage::class;
  protected $npmPackagesDataType = 'array';
  protected $objectsType = GoogleDevtoolsCloudbuildV1ArtifactObjects::class;
  protected $objectsDataType = '';
  protected $pythonPackagesType = GoogleDevtoolsCloudbuildV1PythonPackage::class;
  protected $pythonPackagesDataType = 'array';

  /**
   * @param GoogleDevtoolsCloudbuildV1GoModule[]
   */
  public function setGoModules($goModules)
  {
    $this->goModules = $goModules;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1GoModule[]
   */
  public function getGoModules()
  {
    return $this->goModules;
  }
  /**
   * @param string[]
   */
  public function setImages($images)
  {
    $this->images = $images;
  }
  /**
   * @return string[]
   */
  public function getImages()
  {
    return $this->images;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1MavenArtifact[]
   */
  public function setMavenArtifacts($mavenArtifacts)
  {
    $this->mavenArtifacts = $mavenArtifacts;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1MavenArtifact[]
   */
  public function getMavenArtifacts()
  {
    return $this->mavenArtifacts;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1NpmPackage[]
   */
  public function setNpmPackages($npmPackages)
  {
    $this->npmPackages = $npmPackages;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1NpmPackage[]
   */
  public function getNpmPackages()
  {
    return $this->npmPackages;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1ArtifactObjects
   */
  public function setObjects(GoogleDevtoolsCloudbuildV1ArtifactObjects $objects)
  {
    $this->objects = $objects;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1ArtifactObjects
   */
  public function getObjects()
  {
    return $this->objects;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1PythonPackage[]
   */
  public function setPythonPackages($pythonPackages)
  {
    $this->pythonPackages = $pythonPackages;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1PythonPackage[]
   */
  public function getPythonPackages()
  {
    return $this->pythonPackages;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsCloudbuildV1Artifacts::class, 'Google_Service_CloudRun_GoogleDevtoolsCloudbuildV1Artifacts');
